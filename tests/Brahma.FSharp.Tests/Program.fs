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
    let k =
        <@
            fun (r: Range1D) (a: int clarray) (s: int clcell) ->
                let i = r.GlobalID0
                s.Value <- a.[i]
        @>

    opencl {
        use! a = ClArray.alloc<int> 512
        use! s = ClCell.toDevice 1

        do! runCommand k <| fun it ->
            it
            <| Range1D(512, 256)
            <| a
            <| s

        return! ClCell.toHost s
    }
    // Utils.openclTranslate k
    |> ClTask.runSync context
    |> printfn "%A"

    0
    // allTests
    // |> runTestsWithCLIArgs [] argv
