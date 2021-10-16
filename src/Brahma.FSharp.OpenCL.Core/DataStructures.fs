namespace Brahma.FSharp.OpenCL

open System

type ClArray<'a when 'a : struct> internal (buffer: ClBuffer<'a>) =
    member internal this.Buffer = buffer

    member this.Length = buffer.Length

    member this.Item
        with get (idx: int) : 'a = FailIfOutsideKernel()
        and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

    interface IClMem with
        member this.Size = (buffer :> IClMem).Size
        member this.Data = (buffer :> IClMem).Data

    interface IBuffer<'a> with
        member this.Memory = (buffer :> IBuffer<_>).Memory
        member this.Length = (buffer :> IBuffer<_>).Length
        member this.ElementSize = (buffer :> IBuffer<_>).ElementSize
        member this.Free() = (buffer :> IBuffer<_>).Free()

    member this.Dispose() = (this :> IDisposable).Dispose()

    override this.ToString() =
        sprintf "%O, %A" (buffer :> IClMem).Data (buffer :> IClMem).Size

type ClCell<'a when 'a : struct> internal (buffer: ClBuffer<'a>) =
    member internal this.Buffer = buffer

    member this.Value
        with get () : 'a = FailIfOutsideKernel()
        and set (value: 'a) = FailIfOutsideKernel()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

    interface IClMem with
        member this.Size = (buffer :> IClMem).Size
        member this.Data = (buffer :> IClMem).Data

    interface IBuffer<'a> with
        member this.Memory = (buffer :> IBuffer<_>).Memory
        member this.Length = (buffer :> IBuffer<_>).Length
        member this.ElementSize = (buffer :> IBuffer<_>).ElementSize
        member this.Free() = (buffer :> IBuffer<_>).Free()

    member this.Dispose() = (this :> IDisposable).Dispose()

// fsharplint:disable-next-line
type clarray<'a when 'a : struct> = ClArray<'a>

// fsharplint:disable-next-line
type clcell<'a when 'a : struct> = ClCell<'a>

module ClArray =
    // or allocate with null ptr and write
    let toDevice (array: 'a[]) = opencl {
        let! context = ClTask.ask

        let buffer = context.CreateClBuffer(array)
        return new ClArray<'a>(buffer)
    }

    let alloc<'a when 'a : struct> (size: int) = opencl {
        let! context = ClTask.ask

        let buffer = context.CreateClBuffer(size)
        return new ClArray<'a>(buffer)
    }

    let toHost (clArray: ClArray<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> clArray.Length
        return context.Provider.CommandQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(clArray.Buffer, array, ch))
    }

    // TODO impl it using clEnqueCopy
    let copy (clArray: ClArray<'a>) = opencl {
        failwith "Not implemented yet"
    }

    // TODO impl it
    let copyTo (destination: ClArray<'a>) (source: ClArray<'a>) = opencl {
        failwith "Not implemented yet"
    }

    let close (clArray: ClArray<'a>) = opencl {
        let! ctx = ClTask.ask
        ctx.Provider.CommandQueue.Post <| Msg.CreateFreeMsg(clArray)
    }

module ClCell =
    let toDevice (value: 'a) = opencl {
        let! context = ClTask.ask

        let buffer = context.CreateClBuffer([| value |])
        return new ClCell<'a>(buffer)
    }

    let alloc<'a when 'a : struct> () = opencl {
        let! context = ClTask.ask

        let buffer = context.CreateClBuffer(1, allocationMode = AllocationMode.AllocHostPtr)
        return new ClCell<'a>(buffer)
    }

    let toHost (clCell: ClCell<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> 1
        return context.Provider.CommandQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(clCell.Buffer, array, ch)).[0]
    }

    // TODO impl it
    let copy (clCell: ClCell<'a>) = opencl {
        failwith "Not implemented yet"
    }

[<AutoOpen>]
module ClContextExnetsions =
    type ClContext with
        member this.CreateClArray
            (
                data: 'a[],
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let hostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
            let deviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
            let allocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode

            let buffer = this.CreateClBuffer(data, hostAccessMode = hostAccessMode, deviceAccessMode = deviceAccessMode, allocationMode = allocationMode)
            new ClArray<_>(buffer)

        member this.CreateClArray
            (
                size: int,
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let hostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
            let deviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
            let allocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode

            let buffer = this.CreateClBuffer(size, hostAccessMode = hostAccessMode, deviceAccessMode = deviceAccessMode, allocationMode = allocationMode)
            new ClArray<_>(buffer)

        member this.CreateClCell
            (
                data: 'a,
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let hostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
            let deviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
            let allocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode

            let buffer = this.CreateClBuffer([| data |], hostAccessMode = hostAccessMode, deviceAccessMode = deviceAccessMode, allocationMode = allocationMode)
            new ClCell<_>(buffer)

        member this.CreateClCell
            (
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let hostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
            let deviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
            let allocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode

            let buffer = this.CreateClBuffer(1, hostAccessMode = hostAccessMode, deviceAccessMode = deviceAccessMode, allocationMode = allocationMode)
            new ClCell<_>(buffer)
