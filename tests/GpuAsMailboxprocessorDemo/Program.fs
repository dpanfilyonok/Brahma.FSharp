open GraphBLAS.FSharp.Backend.COOMatrix
open Brahma.FSharp
open GraphBLAS.FSharp.Backend.Common

let m1 =
    {
        Rows = [| 1;2;3 |]
        Columns = [| 1;2;3 |]
        Values = [| 1;2;3 |]
        RowCount = 4
        ColumnCount = 4
    }

let m2 =
    {
        Rows = [| 1;2;3 |]
        Columns = [| 1;2;3 |]
        Values = [| 1;2;3 |]
        RowCount = 4
        ColumnCount = 4
    }

[<EntryPoint>]
let main argv =
    // opencl {
    //     use! clm1 = m1.ToDevice()
    //     use! clm2 = m2.ToDevice()
    //     use! a = EWiseAdd.run clm1 clm2 <@ (+) @>
    //     return a.ToHost()
    // }
    opencl {
        // let kernel =
        //     <@
        //         fun (range: Range1D) (a: int[]) ->
        //             let gid = range.GlobalID0
        //             a.[gid] <- 1
        //     @>

        // let! ctx = ClTask.ask
        // let kernel = ctx.CreateKernel kernel
        // let a = ctx.Allocate<int>(128, HostAccessMode.NotAccessible, AllocationMode.CopyHostPtr, DeviceAccessMode.ReadWrite)

        // ctx.CommandQueue.Post(Msg.MsgSetArguments(fun () ->
        //     kernel.SetArguments
        //         (Range1D(128))
        //         a
        // ))

        let kernel =
            <@
                fun (range: Range1D) (a: int[]) ->
                    let gid = range.GlobalID0
                    a.[gid] <- 1
            @>

        let! ctx = ClTask.ask

        let kernel = ctx.CreateKernel kernel
        let a = ctx.Allocate<int>(128, deviceAccessMode = DeviceAccessMode.ReadWrite, hostAccessMode = HostAccessMode.ReadWrite, allocationMode = AllocationMode.AllocHostPtr)
        ctx.CommandQueue.Post(Msg.MsgSetArguments(fun () ->
            kernel.SetArguments
                (Range1D(128))
                a
        ))

        // let! a = ClArray.toDevice (Array.zeroCreate<int> 128)

        // do! runCommand kernel <| fun x ->
        //     x
        //     <| Range1D(128)
        //     <| a
        // use! a = ClArray.toDevice [| 1;2;3 |]
        // use! b = ClArray.alloc<int> 1
        // let! s, s1 = PrefixSum.runExcludeInplace a b
        ctx.CommandQueue.Post(Msg.CreateRunMsg<_,_,_>(kernel))

        // let! a = ClArray.toDevice (Array.zeroCreate<int> 128)

        // do! runCommand kernel <| fun x ->
        //     x
        //     <| Range1D(128)
        //     <| a
        // use! a = ClArray.toDevice [| 1;2;3 |]
        // use! b = ClArray.alloc<int> 1
        // let! s, s1 = PrefixSum.runExcludeInplace a b
        return a
    }
    |> ClTask.runSync (ClContext(deviceType = ClDeviceType.GPU))
    |> printfn "%A"

    0
