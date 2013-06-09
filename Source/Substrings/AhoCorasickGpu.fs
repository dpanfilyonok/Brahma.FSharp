﻿module AhoCorasickGpu

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks

let provider = NaiveSearchGpu.provider

let createQueue() = 
    new CommandQueue(provider, provider.Devices |> Seq.head) 

let commandQueue = createQueue()

let label = "OpenCL/AhoCorasick"
let mutable timer = null

let buildStateMachine templates maxTemplateLength (next:array<array<int16>>) =
    let go = Array.init (templates * (int) maxTemplateLength) (fun _ -> Array.init 256 (fun _ -> -1s))
    let link = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)
    let parent = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)
    let parentChar = Array.init (templates * (int) maxTemplateLength) (fun _ -> 0uy)

    let rec goFunction v c =
        let rec getLink v =
            if link.[(int) v] < 0s then
                if v = 0s || parent.[(int) v] = 0s then
                    link.[(int) v] <- 0s
                else
                    link.[(int) v] <- goFunction (getLink parent.[(int) v]) parentChar.[(int) v]
            link.[(int) v]

        if go.[(int) v].[(int) c] < 0s then
            if next.[(int) v].[(int) c] >= 0s then
                go.[(int) v].[(int) c] <- next.[(int) v].[(int) c]
            else
                if v = 0s then
                    go.[(int) v].[(int) c] <- 0s
                else
                    go.[(int) v].[(int) c] <- goFunction (getLink v) c
        go.[(int) v].[(int) c]

    let rec findParents v =
        for c in 0..255 do
            if next.[(int) v].[c] >= 0s then
                parent.[(int) next.[(int) v].[c]] <- v
                parentChar.[(int) next.[(int) v].[c]] <- (byte) c
                findParents next.[(int) v].[c]

    let rec processVertex v =
        for c in 0..255 do
            ignore (goFunction v ((byte) c))
            if next.[(int) v].[c] >= 0s then
                processVertex next.[(int) v].[c]

    findParents 0s
    processVertex 0s

    let goPlane = Array.zeroCreate (templates * (int) maxTemplateLength * 256)

    let mutable index = 0
    for i in 0..(templates * (int) maxTemplateLength - 1) do
        for j in 0..255 do
            goPlane.[256 * i + j] <- go.[i].[j]

    goPlane, link

let command = 
    <@
        fun (rng:_1D) l k templates (lengths:array<byte>) (go:array<int16>) (link:array<int16>) (leaf:array<int16>) maxLength (input:array<byte>) (t:array<byte>) (result:array<int16>) ->
            let r = rng.GlobalID0
            let mutable _start = r * k
            let mutable _end = _start + k + (int) maxLength - 1
            if _end > l then _end <- l

            let localTemplateLengths = local (Array.zeroCreate 512)

            let groupSize = 512
            let chunk = (512 + groupSize - 1) / groupSize
            let id = rng.LocalID0

            let upperBound = (id + 1) * chunk
            let mutable higherIndex = upperBound - 1
            if upperBound > 512 then
                higherIndex <- 512 - 1

            for index in (id * chunk)..higherIndex do
                localTemplateLengths.[index] <- lengths.[index]

            for i in _start .. (_end - 1) do
                result.[i] <- -1s

            barrier()

            let mutable v = 0s
            for i in _start .. (_end - 1) do
                if _start - i = 65 then
                    barrier()

                v <- go.[256 * (int) v + (int) input.[i]]
                let mutable parent = v

                while parent > 0s do
                    let mutable currentTemplate = leaf.[(int) parent]
                    if currentTemplate >= 0s then
                        let position = i - (int) localTemplateLengths.[(int) currentTemplate] + 1
                        if result.[position] < currentTemplate then
                            result.[position] <- currentTemplate
                    parent <- link.[(int) parent]
    @>

let mutable result = null
let mutable kernel = null
let mutable kernelPrepare = Unchecked.defaultof<_>
let mutable kernelRun = Unchecked.defaultof<_>
let mutable input = null
let mutable buffersCreated = false

let close () =     
    provider.CloseAllBuffers()
    commandQueue.Dispose()
    provider.Dispose()
    buffersCreated <- false

let initialize length maxTemplateLength k localWorkSize templates templatesSum (templateLengths:array<byte>) (gpuArr:array<byte>) (templateArr:array<byte>) (next:array<array<int16>>) (leaf:array<int16>) =
    timer <- new Timer<string>()
    timer.Start()
    result <- Array.zeroCreate length
    let go, link = buildStateMachine templates maxTemplateLength next
    let l = (length + (k-1))/k 
    let x, y, z = provider.Compile command
    kernel <- x
    kernelPrepare <- y
    kernelRun <- z
    input <- gpuArr
    let d =(new _1D(l,localWorkSize))
    kernelPrepare d length k templates templateLengths go link leaf maxTemplateLength input templateArr result
    timer.Lap(label)
    ()

let mutable ready = true

let upload () =
    if not ready then failwith "Already running, can't upload!"
    ready <- false

    timer.Start()
    Timer<string>.Global.Start()
    if buffersCreated || (provider.AutoconfiguredBuffers <> null && provider.AutoconfiguredBuffers.ContainsKey(input)) then
        ignore (commandQueue.Add(input.ToGpu provider).Finish())
        async {
            ignore (commandQueue.Add(kernelRun()).Finish())
        } |> Async.StartAsTask
    else
        ignore (commandQueue.Add(kernelRun()).Finish())
        async {
            ()
        } |> Async.StartAsTask

let download (task:Task<unit>) =
    if ready then failwith "Not running, can't download!"
    ready <- true

    task.Wait()

    ignore (commandQueue.Add(result.ToHost provider).Finish())
    buffersCreated <- true
    Timer<string>.Global.Lap(label)
    timer.Lap(label)

    result

let findMatches length maxTemplateLength k localWorkSize templates templatesSum (templateLengths:array<byte>) (gpuArr:array<byte>) (templateArr:array<byte>) (next:array<array<int16>>) (leaf:array<int16>) =
    timer.Start()
    
    let go, link = buildStateMachine templates maxTemplateLength next

    let result = Array.zeroCreate length
    let kernelHashed, kernelPrepareHashed, kernelRunHashed = provider.Compile(query=command, translatorOptions=[BoolAsBit])
    let l = (length + (k-1))/k  
    let d =(new _1D(l,localWorkSize))
    kernelPrepareHashed d length k templates templateLengths go link leaf maxTemplateLength (Array.copy gpuArr) templateArr result
    Timer<string>.Global.Start()
    let _ = commandQueue.Add(kernelRunHashed()).Finish()
    let _ = commandQueue.Add(result.ToHost provider).Finish()
    Timer<string>.Global.Lap(label)
    timer.Lap(label)
    result
