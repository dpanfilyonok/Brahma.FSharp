module Brahma.FSharp.OpenCL.Workflow.Evaluation

open Brahma.OpenCL
open OpenCL.Net

exception EmptyDevicesException of string

type OpenCLEvaluationContext(provider: ComputeProvider) =
    member this.Provider = provider

    member this.CommandQueue =
        if provider.Devices |> Seq.isEmpty then
            raise (EmptyDevicesException <| sprintf "Provider:\n%AIt hasn't any device." provider)

        new Brahma.OpenCL.CommandQueue(provider, provider.Devices |> Seq.head)

    new (?platform_name, ?device_type: DeviceType) =
            let platform_name = defaultArg platform_name "*"
            let device_type = defaultArg device_type DeviceType.Default
            let provider = ComputeProvider.Create(platform_name, device_type)
            OpenCLEvaluationContext(provider)

type OpenCLEvaluation<'a> =
    OpenCLEvaluation of (OpenCLEvaluationContext -> 'a)

let private runEvaluation (OpenCLEvaluation f) = f

type OpenCLEvaluationBuilder() =
    abstract member Return : 'a -> OpenCLEvaluation<'a>

    abstract member ReturnFrom : OpenCLEvaluation<'a> -> OpenCLEvaluation<'a>

    abstract member Bind : OpenCLEvaluation<'a> * ('a -> OpenCLEvaluation<'b>) -> OpenCLEvaluation<'b>

    abstract member Zero : unit -> OpenCLEvaluation<unit>

    abstract member TryWith : OpenCLEvaluation<'a> * (exn -> OpenCLEvaluation<'a>) -> OpenCLEvaluation<'a>

    default this.Return x =
        OpenCLEvaluation <| fun _ -> x

    default this.ReturnFrom m = m

    default this.Bind(m, k) =
        OpenCLEvaluation <| fun env ->
            let res = runEvaluation m env
            runEvaluation <| k res <| env

    default this.Zero () =
        this.Return ()

    default this.TryWith (tryBlock, handler) =
        OpenCLEvaluation <| fun env ->
            try
                runEvaluation tryBlock env
            with
            | e ->
                runEvaluation (handler e) env
