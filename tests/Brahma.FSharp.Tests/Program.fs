open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL

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


[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv

    // let command =
    //     <@
    //         fun (range: Range1D) (inBuf:ClArray<int>) (outBuf:ClArray<int>) ->
    //             let i = range.GlobalID0
    //             outBuf.[i] <- inBuf.[i]
    //     @>


    // opencl {
    //     let a = [|1;2;3;4|]
    //     let b = [|0; 0; 0; 0|]
    //     use! inBuf = ClArray.toDevice a
    //     use! outBuf = ClArray.toDevice b
    //     do! runCommand command <| fun x ->
    //         x (Range1D(4)) inBuf outBuf

    //     return! ClArray.toHost outBuf
    // }
    // |> ClTask.runSync context
    // |> printfn "%A"

    // 0

// module internal Utils =
//     let defaultWorkGroupSize = 256

//     let getDefaultGlobalSize n =
//         let m = n - 1
//         m - m % defaultWorkGroupSize + defaultWorkGroupSize

// module internal rec Copy =
//     let copyArray (inputArray: 'a[]) =
//         if inputArray.Length = 0 then
//             opencl { return [||] }
//         else
//             copyNonEmpty inputArray

//     let private copyNonEmpty (inputArray: 'a[]) = opencl {
//         let inputArrayLength = inputArray.Length
//         let copy =
//             <@
//                 fun (ndRange: Range1D)
//                     (inputArrayBuffer: 'a[])
//                     (outputArrayBuffer: 'a[]) ->

//                     let i = ndRange.GlobalID0
//                     if i < inputArrayLength then
//                         outputArrayBuffer.[i] <- inputArrayBuffer.[i]
//             @>

//         let outputArray = Array.zeroCreate inputArray.Length

//         do! RunCommand copy <| fun kernelPrepare ->
//             let ndRange = Range1D(Utils.getDefaultGlobalSize inputArray.Length, Utils.defaultWorkGroupSize)
//             kernelPrepare ndRange inputArray outputArray

//         return outputArray
//     }

// module internal Utils =
//     let defaultWorkGroupSize = 256

//     let getDefaultGlobalSize n =
//         let m = n - 1
//         m - m % defaultWorkGroupSize + defaultWorkGroupSize

// [<EntryPoint>]
// let main argv =
//     let inputArray = Array.create 100_000 1
//     let inputArrayLength = inputArray.Length
//     let copy =
//         <@
//             fun (ndRange: Range1D)
//                 (inputArrayBuffer: int clarray)
//                 (outputArrayBuffer: int clarray) ->

//                 let i = ndRange.GlobalID0
//                 if i < inputArrayLength then
//                     outputArrayBuffer.[i] <- inputArrayBuffer.[i]
//         @>

//     opencl {
//         use! a = ClArray.ToDevice(inputArray, allocationMode = AllocationMode.UseHostPtr)
//         use! b = ClArray.alloc<int> 100_000
//         do! runCommand copy <| fun x ->
//             x
//             <| Range1D.CreateValid(inputArray.Length, Utils.defaultWorkGroupSize)
//             <| a
//             <| b

//         return! ClArray.toHost b
//     }
//     |> ClTask.runSync (ClContext(deviceType = ClDeviceType.CPU))
//     |> printfn "%A"

//     0
    // let kernel =
    //     <@
    //         fun (range: Range1D) (a: int clarray) ->
    //             let gid = range.GlobalID0
    //             a.[gid] <- 1
    //     @>

    // opencl {
    //     use! a = ClArray.alloc<int> 128
    //     do! runCommand kernel <| fun x ->
    //         x
    //         <| Range1D(128, 128)
    //         <| a

    //     // use res =
    //     //     opencl {
    //     //         return
    //     //             { new System.IDisposable with
    //     //                 member this.Dispose() = ()
    //     //             }
    //     //     }
    //     // let! s = opencl {return res}
    //     return! ClArray.toHost a

    // }
    // |> ClTask.runSync (ClContext(deviceType = ClDeviceType.CPU))
    // |> printfn "%A"

    // let kernel = gpu.CreateKernel copy
    // let a = gpu.Allocate<bool>(inputArray)
    // let b = gpu.Allocate<bool>(100_000)

    // let p = gpu.GetNewProcessor()
    // p.Post <| Msg.MsgSetArguments(fun () -> kernel.SetArguments (Range1D(Utils.getDefaultGlobalSize inputArray.Length, Utils.defaultWorkGroupSize)) a b)
    // p.Post <| Msg.CreateRunMsg(kernel)
    // p.Post <| Msg.CreateToHostMsg(b, inputArray)
    // p.PostAndReply(fun ch -> Msg.MsgNotifyMe ch)
    // printfn "%A" inputArray



    // 0

// [<EntryPoint>]
// let main argv =
//     let kernel =
//         <@
//             fun (range: Range1D) (a: Buffer<int>) ->
//                 let gid = range.GlobalID0
//                 a.[gid] <- 1
//         @>

//     let kernel = gpu.CreateKernel kernel
//     let a = gpu.Allocate<int>(128, deviceAccessMode = DeviceAccessMode.ReadWrite, hostAccessMode = HostAccessMode.ReadWrite, allocationMode = AllocationMode.AllocHostPtr)
//     let p = gpu.GetNewProcessor()
//     p.Post(Msg.MsgSetArguments(fun () ->
//         kernel.SetArguments
//             (Range1D(128))
//             a
//     ))

//     // let! a = ClArray.toDevice (Array.zeroCreate<int> 128)

//     // do! runCommand kernel <| fun x ->
//     //     x
//     //     <| Range1D(128)
//     //     <| a
//     // use! a = ClArray.toDevice [| 1;2;3 |]
//     // use! b = ClArray.alloc<int> 1
//     // let! s, s1 = PrefixSum.runExcludeInplace a b
//     p.Post(Msg.CreateRunMsg<_,_>(kernel))
//     let ass = Array.zeroCreate<int> 128
//     p.Post <| Msg.CreateToHostMsg(a, ass)
//     p.PostAndReply(fun ch -> Msg.MsgNotifyMe ch)
//     printfn "%A" ass
//     // p.Post

//     // let! a = ClArray.toDevice (Array.zeroCreate<int> 128)

//     // do! runCommand kernel <| fun x ->
//     //     x
//     //     <| Range1D(128)
//     //     <| a
//     // use! a = ClArray.toDevice [| 1;2;3 |]
//     // use! b = ClArray.alloc<int> 1
//     // let! s, s1 = PrefixSum.runExcludeInplace a b
//     // return a

//     // let s = gpu.CreateKernel k

//     // let a = gpu.Allocate<bool>(1)
//     // let b = gpu.Allocate<bool>(1)

//     // p.Post <| Msg.MsgSetArguments(fun () -> s.SetArguments (Range1D(256, 256)) b a)
//     // p.Post <| Msg.CreateRunMsg(s)
//     // p.PostAndReply <| (fun ch -> Msg.MsgNotifyMe ch)
//     // printfn "Lol"

//     // p.Post <| Msg.CreateT

//     0

// TODO тест на булы с копирование
// TODO тест на сложные операции с булами
// TODO тест на простые операции с булами
// TODO реализация копирования массивов
// TODO проверка что в транляторе все работает
// TODO проверка что в кернеле все верно
// TODO проверка что в буфере все верно
// TODO прверить синхронизацию при toHost
// TODO подумать на апи ClArray
// TODO тест на use!
// TODO parallel
