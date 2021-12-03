open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL

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
    // let command =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             atomic inc buf.[0] |> ignore
    //     @>

    // Utils.openclTranslate command
    // |> printfn "%A"
    // 0

    allTests
    |> runTestsWithCLIArgs [] argv
