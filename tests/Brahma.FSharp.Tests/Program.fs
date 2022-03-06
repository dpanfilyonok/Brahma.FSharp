open Expecto

open Brahma.FSharp.Tests

[<Tests>]
let allTests =
    testList "All tests" [
        Full.tests
        Workflow.tests
        Translator.tests
        QuotationTransformers.tests
        CompositeTypesTests.tests
        // Union.tests
        // Atomic.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv
