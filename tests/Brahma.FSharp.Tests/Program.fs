open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL

[<Tests>]
let allTests =
    testList "All tests" [
        Full.tests
        Translator.tests
        // Atomic.tests
        StructAndTuple.tests
        Workflow.tests
        QuotationTransformers.tests
        Union.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv
