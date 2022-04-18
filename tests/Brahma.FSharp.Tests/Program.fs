open Expecto
open TranslatorTests
open Brahma.FSharp

[<Tests>]
let allTests =
    testList "All tests" [
        testList "Translation tests" TranslationTests.tests
        testList "Execution tests" ExecutionTests.tests
    ]
    |> testSequenced


[<Struct>]
type Un =
    |A of float32
    |B of int * float

[<EntryPoint>]
let main argv =
//    let k =
//        <@
//            fun (r: Range1D)
//                (k: Un clarray) ->
//
//                k.[0] <-
//                    match k.[0] with
//                    | A a -> A 5.f
//                    | B (a, b) -> A 5.f
//        @>

    let k =
        <@
            fun (r: Range1D)
                (k: int clarray) ->

                k.[0] <-
                    match Some 1 with
                    | Some a -> a
                    | None -> 5

        @>

    k
    |> openclTranslate TranslationTests.translators.Head
    |> printfn "%A"

    0
//
//    allTests
//    |> runTestsWithCLIArgs [] argv
