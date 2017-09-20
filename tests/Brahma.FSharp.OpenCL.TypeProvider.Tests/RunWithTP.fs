module Brahma.FSharp.OpenCL.TypeProvider.Tests.Runner

open System.IO
open NUnit.Framework
open OpenCL.Net
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.TypeProvider.Provided
open Brahma.Helpers
open Brahma.OpenCL

let [<Literal>] clSourcePath = "../Brahma.FSharp.OpenCL/OpenCLSources/assignArr.cl"

type Provided = KernelProvider<clSourcePath, TreatPointersAsArrays=true>

[<TestFixture>]
type RunWithTP() =
    let length = 8
    let gpuArr = Array.init length (fun _ -> 0)

    let Run platformName (arr:array<_>) =
        let localWorkSize = 2
        let deviceType = DeviceType.Default

        let additionalClSource = System.IO.File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, clSourcePath))

        let computeProvider =
            try ComputeProvider.Create(platformName, deviceType)
            with
            | ex -> failwith ex.Message
        let mutable commandQueue = new CommandQueue(computeProvider, computeProvider.Devices |> Seq.head)

        let a = arr
        let v = 42
        let command =
            <@
                fun (r:_1D) (a:array<_>) (v:int) ->
                    Provided.assignArr(a, v)
            @>

        let kernel, kernelPrepare, kernelRun =
            computeProvider.Compile(command, _additionalSources = [additionalClSource])
        let d =(new _1D(length, localWorkSize))
        kernelPrepare d a v

        let _ = commandQueue.Add(kernelRun()).Finish()
        let _ = commandQueue.Add(a.ToHost computeProvider).Finish()

        let isSuccess = Array.fold (fun flag v -> flag && v = 42) true a

        commandQueue.Dispose()
        computeProvider.Dispose()
        computeProvider.CloseAllBuffers()

        isSuccess
    
    [<Test>]
    member this.``Run code with TP on GPU``() =
        let res = Run "*" gpuArr
        Assert.True(res)
