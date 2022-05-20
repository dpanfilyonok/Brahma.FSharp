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
//    let a =
//        <@
//            fun (range: Range1D) (acc: int clcell) ->
//                atomic (fun x -> x + 1) acc.Value |> ignore
//        @>
//    TranslatorTests.Helpers.openclTranslate TranslationTests.translators.Head a
//    |> printfn "%A"
//    0
    allTests
    |> runTestsWithCLIArgs [] argv
