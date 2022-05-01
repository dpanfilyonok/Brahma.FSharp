open Expecto
open Brahma.FSharp

[<Tests>]
let allTests =
    testList "All tests" [
        testList "Translation tests" TranslationTests.tests
        testList "Execution tests" ExecutionTests.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv
