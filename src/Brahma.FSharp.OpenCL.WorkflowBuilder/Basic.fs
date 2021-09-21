namespace Brahma.FSharp.OpenCL.WorkflowBuilder

open FSharp.Quotations
//open Brahma.FSharp.OpenCL.Extensions
//open Brahma.FSharp.OpenCL.Core

//TODO: redesign me

(*
[<AutoOpen>]
module OpenCLEvaluationContextExtensions =
    type OpenCLEvaluationContext with
        member this.RunSync (OpenCLEvaluation f) =
            let res = f this
            this.CommandQueue.Finish() |> ignore
            res

        member this.RunAsync (OpenCLEvaluation f) =
            fun () -> this.RunSync <| OpenCLEvaluation f

[<AutoOpen>]
module ClEvaluation =
    let opencl = OpenCLEvaluationBuilder()

    let getEvaluationContext : OpenCLEvaluation<OpenCLEvaluationContext> =
        OpenCLEvaluation id

    /// If array is associated with gpu memory, then transfers it to host memory and returns a ordinary F# array.
    /// Otherwise simply returns the array passed to it, like the identity function.
    let toHost (xs: 'a[]) : OpenCLEvaluation<array<'a>> =
        opencl {
            let! ctx = getEvaluationContext

            if ctx.Provider.AutoconfiguredBuffers.ContainsKey xs then
                ctx.CommandQueue.Add(xs.ToHost ctx.Provider) |> ignore

            return xs
        }

    let transferToHost (array: 'a[]) : OpenCLEvaluation<unit> =
        opencl {
            let! _ = toHost array
            return ()
        }

    let runCommand (command: Expr<'range -> 'a>) (binder: ('range -> 'a) -> unit) : OpenCLEvaluation<unit> =
        opencl {
            let! ctx = getEvaluationContext

            let (_, kernelPrepare, kernelRun) =
                if not ctx.IsCachingEnabled then
                    ctx.Provider.Compile command
                else
                    match ctx.CompilingCache.TryGetValue <| ExprWrapper command.Raw with
                    | true, (kernel, kernelPrepare, kernelRun) ->
                        unbox<Brahma.OpenCL.Kernel<'range>> kernel,
                        unbox<'range -> 'a> kernelPrepare,
                        unbox<unit -> Brahma.OpenCL.Commands.Run<'range>> kernelRun
                    | false, _ ->
                        let (kernel, kernelPrepare, kernelRun) = ctx.Provider.Compile command
                        ctx.CompilingCache.Add(ExprWrapper command.Raw, (box kernel, box kernelPrepare, box kernelRun))
                        kernel, kernelPrepare, kernelRun

            binder kernelPrepare
            ctx.CommandQueue.Add(kernelRun()) |> ignore
        }
*)
