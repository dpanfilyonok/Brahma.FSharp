module Brahma.FSharp.OpenCL.Workflow.BasicEvaluations

open FSharp.Quotations

open Brahma.FSharp.OpenCL.Workflow.BasicMonads.Reader
open Brahma.FSharp.OpenCL.Workflow.Workflow

open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Core

let evaluation = EvaluationBuilder()

let getContext = ask

let ToHost (xs : array<'a>) : Reader<OpenCLContext, array<'a>> =
    evaluation {
        let! ctx = getContext
        ctx.CommandQueue.Add(xs.ToHost ctx.Provider).Finish() |> ignore
        return xs
    }

let RunCommand (command : Expr<'range -> 'a>) (binder : ('range -> 'a) -> unit) : Reader<OpenCLContext, unit> =
    evaluation {
        let! ctx = getContext
        let _, kernelP, kernelR = ctx.Provider.Compile command

        binder kernelP
        ctx.CommandQueue.Add(kernelR()).Finish() |> ignore
    }
