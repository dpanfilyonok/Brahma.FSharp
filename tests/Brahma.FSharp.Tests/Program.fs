open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL

[<Tests>]
let allTests =
    testList "All tests" [
        // Full.tests
        // Workflow.tests
        // Translator.tests
        // QuotationTransformers.tests
        CompositeTypesTests.tests
        // Union.tests
        // Atomic.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let tempVarX = 1
                max buf.[0] tempVarX |> ignore
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let tempVarX = 1
                buf.[0] <- max buf.[0] tempVarX
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let tempVarY = 1
                buf.[0] <- max buf.[0] tempVarY
                buf.[0] <- max buf.[0] tempVarY
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let gid = range.GlobalID0
                buf.[gid] <- max (atomic (fun x -> x + 1) buf.[gid]) (if 1 > 0 then 100 else -100)
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let gid = range.GlobalID0
                atomic (fun x -> x + 1) buf.[gid] |> ignore
        @>

    // let kernel =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             let tempVarY = 1.
    //             buf.[0] <- atomic (+) buf.[0] tempVarY
    //     @>

    // let kernel =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             while true do
    //                 buf.[0] <- 1.
    //     @>

    // let kernel =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             let f (x: int) =
    //                 x |> ignore
    //                 x

    //             f 6
    //     @>

    // opencl {
    //     do! runCommand kernel (fun kernelPrepare -> kernelPrepare <| Range1D(10))
    // }
    // |> ClTask.runSync context
    // Utils.openclTransformQuotation kernel
    // |> printfn "%A"
    // Utils.openclTranslate kernel
    // |> printfn "%A"
    // 0
    allTests
    |> runTestsWithCLIArgs [] argv
