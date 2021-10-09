open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL

// [<Tests>]
// let allTests =
//     testList "All tests" [
//         Translator.tests
//         Full.tests
//         //Atomic.tests
//         //Workflow.tests
//         QuotationTransformers.tests
//         Union.tests
//     ]
//     |> testSequenced

[<EntryPoint>]
let main argv =
    // printfn "%O\n" gpu

    // allTests
    // |> runTestsWithCLIArgs [] argv
    let k =
        <@
            fun (r: _1D) (b: Buffer<bool>) (a: Buffer<bool>) ->
                let s = b.[0] && b.[0]
                a.[0] <- s
        @>

    // Utils.openclCompile k
    // |> printfn "%A"

    let s = gpu.CreateKernel k
    let p = gpu.GetNewProcessor()

    let a = gpu.Allocate<bool>(1)
    let b = gpu.Allocate<bool>(1)

    p.Post <| Msg.MsgSetArguments(fun () -> s.SetArguments (_1D(256, 256)) b a)
    p.Post <| Msg.CreateRunMsg(s)
    p.PostAndReply <| (fun ch -> Msg.MsgNotifyMe ch)
    printfn "Lol"

    // p.Post <| Msg.CreateT

    0
