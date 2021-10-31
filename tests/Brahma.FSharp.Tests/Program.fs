open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL
open FSharp.Quotations

// [<Tests>]
// let allTests =
//     testList "All tests" [
//         Full.tests
//         Translator.tests
//         // Atomic.tests
//         Workflow.tests
//         QuotationTransformers.tests
//         Union.tests
//     ]
//     |> testSequenced

[<EntryPoint>]
let main argv =
    let value = 10
    let command =
        <@
            fun (range: Range1D) (cell: int) ->
                atomic (fun x -> x + value) cell |> ignore
        @>

    // opencl {
    //     use! s = ClCell.toDevice 1

    //     do! runCommand k <| fun it ->
    //         it
    //         <| Range1D(512, 256)
    //         <| s

    //     return! ClCell.toHost s
    // }
    Utils.openclTranslate command
    // |> ClTask.runSync context
    |> printfn "%A"

    0
    // allTests
    // |> runTestsWithCLIArgs [] argv
