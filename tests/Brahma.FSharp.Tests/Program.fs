open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL
open FSharp.Quotations
open System.Runtime.InteropServices

[<Tests>]
let allTests =
    testList "All tests" [
        Full.tests
        Translator.tests
        // Atomic.tests
        Workflow.tests
        QuotationTransformers.tests
        Union.tests
    ]
    |> testSequenced

[<Struct>]
type TestStruct =
    val mutable x: int
    val mutable y: float
    new(x, y) = { x = x; y = y }

    override this.ToString() =
        sprintf "%A, %A" this.x this.y

[<Struct>]
type TestStruct2 =
    [<MarshalAs(UnmanagedType.U1)>] val mutable x: bool
    val mutable y: TestStruct
    new(x, y) = { x = x; y = y }

    override this.ToString() =
        sprintf "%A, %A" this.x this.y

[<EntryPoint>]
let main argv =
    // let command =
    //     <@
    //         fun (range: Range1D) (buf:  ClArray<TestStruct2>) ->
    //             if range.GlobalID0 = 0
    //             then
    //                 let b = buf.[0]
    //                 buf.[0] <- buf.[1]
    //                 buf.[1] <- b
    //     @>


    // opencl {
    //     use! inBuf = ClArray.toDevice [|TestStruct2(true, TestStruct(1, 2.0)); TestStruct2(true, TestStruct(3, 4.0))|]
    //     do! runCommand command <| fun x ->
    //         x (Range1D(5, 5)) inBuf

    //     return! ClArray.toHost inBuf
    // }
    // |> ClTask.runSync context
    // // let value = 10
    let command =
        <@
            fun (range: Range1D) (cell: int clcell) ->
                atomic (fun x -> x + 1) cell.Value |> ignore
        @>

    Utils.openclTranslate command
    |> printfn "%A"

    opencl {
        use! s = ClCell.alloc<int> ()

        do! runCommand command <| fun it ->
            it
            <| Range1D(512, 256)
            <| s

        return! ClCell.toHost s
    }
    // Utils.openclTranslate command
    |> ClTask.runSync context
    |> printfn "%A"

    0
    // let s =
    //     testProperty "Parallel execution of kernel" <| fun _const ->
    //         let n = 4
    //         let l = 256
    //         let getAllocator (context:ClContext)  =
    //              let kernel =
    //                  <@
    //                      fun (r: Range1D) (buffer: ClArray<int>) ->
    //                          let i = r.GlobalID0
    //                          buffer.[i] <- _const
    //                  @>
    //              let k = context.CreateClKernel kernel
    //              fun (q:MailboxProcessor<_>) ->
    //                  let buf = context.CreateClArray(l, allocationMode = AllocationMode.AllocHostPtr)
    //                  q.Post(Msg.MsgSetArguments(fun () -> k.ArgumentsSetter (Range1D(l, l)) buf))
    //                  q.Post(Msg.CreateRunMsg<_,_>(k))
    //                  buf

    //         let allocator = getAllocator context
    //         let allocOnGPU (q:MailboxProcessor<_>) allocator =
    //             let b = allocator q
    //             let res = Array.zeroCreate l
    //             q.PostAndReply (fun ch -> Msg.CreateToHostMsg(b, res, ch))
    //             q.Post (Msg.CreateFreeMsg b)
    //             res


    //         let actual =
    //             Array.init n (fun _ ->
    //                     let q = context.CommandQueue
    //                     q)
    //             |> Array.mapi (fun i q -> async {return allocOnGPU q allocator})
    //             |> Async.Parallel
    //             |> Async.RunSynchronously

    //         let expected = Array.init n (fun _ -> Array.create l _const)

    //         Expect.sequenceEqual actual expected "Arrays should be equals"
    // s
    // |> runTestsWithCLIArgs [] argv
