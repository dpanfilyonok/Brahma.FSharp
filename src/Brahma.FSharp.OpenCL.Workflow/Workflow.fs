module Brahma.FSharp.OpenCL.Workflow.Workflow

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Workflow.BasicMonads.Reader

type OpenCLContext(provider: ComputeProvider) =
    member this.Provider = provider

    member this.CommandQueue =
        new CommandQueue(provider, provider.Devices |> Seq.head)

    member this.RunSync (Reader f) = f this

let Evaluation (f: OpenCLContext -> 'a): Reader<OpenCLContext, 'a> = Reader f

type EvaluationBuilder() =
    inherit ReaderBuilder<OpenCLContext>()
