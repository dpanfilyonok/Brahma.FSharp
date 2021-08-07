open Expecto

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

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv
