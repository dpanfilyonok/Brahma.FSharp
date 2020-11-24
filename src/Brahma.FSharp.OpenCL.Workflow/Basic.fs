module Brahma.FSharp.OpenCL.Workflow.Basic

open FSharp.Quotations

open Brahma.FSharp.OpenCL.Workflow.Evaluation
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Core

let opencl = OpenCLEvaluationBuilder()

let getContext : OpenCLEvaluation<OpenCLContext> =
    OpenCLEvaluation <|fun x -> x

type OpenCLContext with
    member this.RunSync (OpenCLEvaluation f) =
        let res = f this
        this.CommandQueue.Finish() |> ignore
        res

    member this.RunAsync (OpenCLEvaluation f) =
        fun () -> this.RunSync <| OpenCLEvaluation f

let ToHost (xs : array<'a>) : OpenCLEvaluation<array<'a>> =
    opencl {
        let! ctx = getContext
        ctx.CommandQueue.Add(xs.ToHost ctx.Provider) |> ignore
        return xs
    }

let RunCommand (command : Expr<'range -> 'a>) (binder : ('range -> 'a) -> unit) : OpenCLEvaluation<unit> =
    opencl {
        let! ctx = getContext
        let _, kernelP, kernelR = ctx.Provider.Compile command

        binder kernelP
        ctx.CommandQueue.Add(kernelR()) |> ignore
    }
