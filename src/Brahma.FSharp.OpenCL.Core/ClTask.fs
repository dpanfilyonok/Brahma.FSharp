namespace Brahma.FSharp

open OpenCL.Net
open System.Collections.Generic
open FSharp.Quotations

type ClTask<'a> =
    ClTask of (ClContext -> 'a)

type ClTaskBuilder() =
    let runEvaluation (ClTask f) = f

    member this.Bind(m, k) = ClTask <| fun env ->
        let res = runEvaluation m env
        runEvaluation <| k res <| env

    member this.Return x = ClTask <| fun _ ->
        x

    member this.ReturnFrom m = m

    member this.Zero() =
        this.Return()

    member this.Combine(m1, m2) = ClTask <| fun env ->
        runEvaluation m1 env
        runEvaluation m2 env

    member this.Delay(rest) = ClTask <| fun env ->
        runEvaluation (rest()) env

    member this.While(predicate, body) = ClTask <| fun env ->
        while predicate() do
            runEvaluation body env

    member this.For(elems, body) = ClTask <| fun env ->
        for elem in elems do
            runEvaluation (body elem) env

    member this.TryWith(tryBlock, handler) = ClTask <| fun env ->
        try
            runEvaluation tryBlock env
        with
        | e ->
            runEvaluation (handler e) env

module ClTask =
    // let runSync (context: ClContext) (ClTask f) =
    //     let res = f context
    //     context.CommandQueue.Finish() |> ignore
    //     res

    let ask = ClTask id

[<AutoOpen>]
module ClTaskImpl =
    let opencl = ClTaskBuilder()

    // let runCommand (command: Expr<'range -> 'a>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
    //     opencl {
    //         let! ctx = getEvaluationContext

    //         let (_, kernelPrepare, kernelRun) =
    //             if not ctx.IsCachingEnabled then
    //                 ctx.Provider.Compile command
    //             else
    //                 match ctx.CompilingCache.TryGetValue <| ExprWrapper command.Raw with
    //                 | true, (kernel, kernelPrepare, kernelRun) ->
    //                     unbox<Brahma.OpenCL.Kernel<'range>> kernel,
    //                     unbox<'range -> 'a> kernelPrepare,
    //                     unbox<unit -> Brahma.OpenCL.Commands.Run<'range>> kernelRun
    //                 | false, _ ->
    //                     let (kernel, kernelPrepare, kernelRun) = ctx.Provider.Compile command
    //                     ctx.CompilingCache.Add(ExprWrapper command.Raw, (box kernel, box kernelPrepare, box kernelRun))
    //                     kernel, kernelPrepare, kernelRun

    //         binder kernelPrepare
    //         ctx.CommandQueue.Add(kernelRun()) |> ignore
    //     }
