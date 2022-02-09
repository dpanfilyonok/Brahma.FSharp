namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
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

        member this.Item
            with get (idx: int) : 'a = FailIfOutsideKernel()
            and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    member this.Dispose() = (this :> IDisposable).Dispose()

    override this.ToString() =
        $"{(buffer :> IClMem).Data}, %A{(buffer :> IClMem).Size}"

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
        member this.Item
            with get (idx: int) : 'a = FailIfOutsideKernel()
            and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    member this.Dispose() = (this :> IDisposable).Dispose()

// fsharplint:disable-next-line
type clarray<'a when 'a : struct> = ClArray<'a>

// fsharplint:disable-next-line
type clcell<'a when 'a : struct> = ClCell<'a>

// TODO set flags
module ClArray =
    // or allocate with null ptr and write
    // TODO if array.Length = 0 ...
    let toDevice (array: 'a[]) = opencl {
        let! context = ClTask.ask

        let memFlags =
            {
                HostAccessMode = context.RuntimeOptions.HostAccessMode
                DeviceAccessMode = context.RuntimeOptions.DeviceAccessMode
                AllocationMode = context.RuntimeOptions.AllocationModeIfData
            }

        let buffer = new ClBuffer<'a>(context.ClContext, Data array, memFlags)
        return new ClArray<'a>(buffer)
    }

    let alloc<'a when 'a : struct> (size: int) = opencl {
        let! context = ClTask.ask

        let memFlags =
            {
                HostAccessMode = context.RuntimeOptions.HostAccessMode
                DeviceAccessMode = context.RuntimeOptions.DeviceAccessMode
                AllocationMode = context.RuntimeOptions.AllocationModeIfNoData
            }

        let buffer = new ClBuffer<'a>(context.ClContext, Size size, memFlags)
        return new ClArray<'a>(buffer)
    }

    let toHost (clArray: ClArray<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> clArray.Length
        return context.CommandQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(clArray.Buffer, array, ch))
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
        ctx.CommandQueue.Post <| Msg.CreateFreeMsg(clArray)
    }

module ClCell =
    let toDevice (value: 'a) = opencl {
        let! context = ClTask.ask

        let memFlags =
            {
                HostAccessMode = context.RuntimeOptions.HostAccessMode
                DeviceAccessMode = context.RuntimeOptions.DeviceAccessMode
                AllocationMode = context.RuntimeOptions.AllocationModeIfData
            }

        let buffer = new ClBuffer<'a>(context.ClContext, Data [| value |], memFlags)
        return new ClCell<'a>(buffer)
    }

    let alloc<'a when 'a : struct> () = opencl {
        let! context = ClTask.ask

        let memFlags =
            {
                HostAccessMode = context.RuntimeOptions.HostAccessMode
                DeviceAccessMode = context.RuntimeOptions.DeviceAccessMode
                AllocationMode = context.RuntimeOptions.AllocationModeIfNoData
            }

        let buffer = new ClBuffer<'a>(context.ClContext, Size 1, memFlags)
        return new ClCell<'a>(buffer)
    }

    let toHost (clCell: ClCell<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> 1
        return context.CommandQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(clCell.Buffer, array, ch)).[0]
    }

    // TODO impl it
    let copy (clCell: ClCell<'a>) = opencl {
        failwith "Not implemented yet"
    }
