module Brahma.FSharp.OpenCL.Workflow.Evaluation

open Brahma.OpenCL

type OpenCLContext(provider: ComputeProvider) =
    member this.Provider = provider

    member this.CommandQueue =
        new CommandQueue(provider, provider.Devices |> Seq.head)

type OpenCLEvaluation<'a> =
    OpenCLEvaluation of (OpenCLContext -> 'a)

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
