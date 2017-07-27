module Brahma.Agents.Hopac

open Brahma.Helpers
open OpenCL.Net
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.OpenCL
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open System.Diagnostics


type Msg<'data,'res> =
    | Die of IVar<unit>
    | Process of 'data*('res -> unit)
    | PostProcess of 'res
    | Fill of 'data*(Option<'data> -> unit)
    | InitBuffers of array<'data>*IVar<array<'data>>
    | Get of IVar<Option<'data>>
    | Enq of 'data

type Reader<'d>  (fillF:'d -> Option<'d>) = 
    
    member this.Create : Job<Ch<Msg<'d,_>>>= job {
        let inCh = Ch() 
        let loop = job {
            let! msg = Ch.take inCh
            match msg with
            | Die ch -> 
                do! IVar.fill ch ()
                do! Job.abort()
            | Fill (x, cont) ->
                let filled = fillF x
                cont filled
                if filled.IsNone
                then do! inCh *<+=>- (fun reply -> Die reply)
            | x -> printfn "unexpected message for reader: %A" x
            }
        do! Job.foreverServer loop
        return inCh
        }
    
    member this.Read inCh (a, cont) = inCh *<+ Fill(a, cont)


type Worker<'d,'r>(f: 'd -> 'r) =
    
    member this.Create = job {
        let inCh = Ch() 
        let loop = job {
            let! msg = Ch.take inCh
            match msg with
            | Die ch ->
                do! IVar.fill ch ()
                do! Job.abort()
            | Process (x,continuation) ->
                let r = f x
                continuation r
            | x -> printfn "unexpected message for Worker: %A" x
            }
        do! Job.foreverServer loop
        return inCh
        }
 
    member this.Process inCh (a, continuation) = inCh *<+ Process(a,continuation)
 
    
type DataManager<'d>(reader:Reader<'d>) =
    member this.Create = job {
        let inCh = Ch()   
        let! rCh = reader.Create   
        let dataToProcess = new System.Collections.Concurrent.ConcurrentQueue<Option<'d>>()
        let dataToFill = new System.Collections.Generic.Queue<_>()
        let dataIsEnd = ref false
        let loop = job {
            while dataToFill.Count > 0 do                           
                let b = dataToFill.Dequeue()
                if not <| !dataIsEnd
                then do! reader.Read rCh (b, fun a -> dataToProcess.Enqueue a
                                                      dataIsEnd := Option.isNone a)
            let! msg = Ch.take inCh
            match msg with
            | Die ch ->
                if !dataIsEnd 
                then 
                    do! IVar.fill ch ()
                    do! Job.abort() 
                else do! inCh *<+=>- (fun reply -> Die reply)
            | InitBuffers (bufs,ch) ->
                do! IVar.fill ch bufs
                bufs |> Array.iter dataToFill.Enqueue                                
            | Get ch -> 
                let s,r = dataToProcess.TryDequeue()
                if s
                then 
                    if r.IsNone 
                    then dataIsEnd := true
                    do! IVar.fill ch r    
                elif not !dataIsEnd
                then do! inCh *<+ Get ch        
                else do! IVar.fill ch None
            | Enq b -> dataToFill.Enqueue b
            | x -> printfn "Unexpected message for Worker: %A" x
        }
        do! Job.foreverServer loop  
        return inCh
    }

    member this.InitBuffers ch bufs = Job.delay <| fun () ->
        let reply = IVar()
        ch *<+ InitBuffers(bufs, reply) >>-. reply
    member this.Enq inCh b = inCh *<+ Enq b
    member this.GetData inCh = inCh *<+=>- (fun reply -> Get reply)


type Master<'d,'r,'fr>(workers:array<Worker<'d,'r>>, fill: 'd -> Option<'d>, bufs:ResizeArray<'d>, postProcessF:Option<'r->'fr>) =             
    
    let isDataEnd = ref false
    member this.Create = job {  
        let reader = new Reader<'d>(fill)     
        
        let dataManager = new DataManager<'d>(reader)
        let! dmCh = dataManager.Create

        let postprocessor = postProcessF |> Option.map(fun f -> new Worker<_,_>(f))
        let mutable ppCh = Ch()
        if postprocessor.IsSome 
        then 
            let! ch = postprocessor.Value.Create
            ppCh <- ch

        let workerch = new ResizeArray<_>()
        for i = 0 to workers.Length - 1 do
            let! iW = workers.[i].Create
            workerch.Add(workers.[i], iW)
        let freeWorkers = new System.Collections.Concurrent.ConcurrentQueue<_>(workerch) 
             
        let inCh = Ch()
        let! buffers = dataManager.InitBuffers dmCh (bufs.ToArray())

        let rec loop = job {   
            if not freeWorkers.IsEmpty
            then 
                let success,w = freeWorkers.TryDequeue()
                if success
                then
                    let! b = dataManager.GetData dmCh
                    if b.IsSome
                    then
                        if postprocessor.IsSome 
                        then
                            do! (fst w).Process (snd w)
                                    (b.Value
                                    , fun a -> run <| job {
                                          do! postprocessor.Value.Process ppCh (a, fun _ -> ()) 
                                          freeWorkers.Enqueue w
                                          do! dataManager.Enq dmCh b.Value}) 
                        else 
                            do! (fst w).Process (snd w)
                                    (b.Value
                                    , fun a -> run <| job {
                                          freeWorkers.Enqueue w
                                          do! dataManager.Enq dmCh b.Value})
                    else 
                        do! dmCh *<+=>- (fun reply -> Die reply)
                        for i = 0 to workerch.Count - 1 do 
                            do! snd workerch.[i] *<+=>- (fun reply -> Die reply)
                        if postprocessor.IsSome 
                        then do! ppCh *<+=>- (fun reply -> Die reply)
                        isDataEnd := true 
                        do! Job.abort() 
            }
        do! Job.foreverServer loop
        return inCh
    }

    member this.IsDataEnd = !isDataEnd