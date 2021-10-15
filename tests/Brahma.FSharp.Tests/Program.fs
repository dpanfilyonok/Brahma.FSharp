open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL
open FSharp.Quotations

[<Tests>]
let allTests =
    testList "All tests" [
        Full.tests
        Translator.tests
        // Atomic.tests
        Workflow.tests
        QuotationTransformers.tests
        Union.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv
