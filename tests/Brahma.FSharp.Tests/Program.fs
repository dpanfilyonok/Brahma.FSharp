open Expecto

open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.WorkflowBuilder
open Brahma.FSharp.Tests

[<Tests>]
let allTests =
    testList "All tests" [
        Translator.tests
        Full.tests
        Atomic.tests
        Workflow.tests
        QuotationTransformers.tests
        Union.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    printfn "%O\n" Utils.context

    allTests
    |> runTestsWithCLIArgs [] argv
