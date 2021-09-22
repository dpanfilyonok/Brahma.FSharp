namespace Brahma.FSharp

open System

type ClArray<'a when 'a : struct>(buffer: ClBuffer<'a>) =
    member internal this.Buffer = buffer

    member this.Length = buffer.Length

    member this.Item
        with get (idx: int) : 'a = failIfOutsideKernel ()
        and set (idx: int) (value: 'a) = failIfOutsideKernel ()

    member this.Dispose() = (this :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

type ClCell<'a when 'a : struct>(buffer: ClBuffer<'a>) =
    member internal this.Buffer = buffer

    // static member inline (!) (cell: ClCell<'a>) : 'a = failIfOutsideKernel ()

    member this.Dispose() = (this :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

module ClArray =
    let toDevice (array: 'a[]) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.Context, Size array.Length)
        context.CommandQueue.Post <| Msg.CreateToHostMsg(buffer, array)
        return new ClArray<'a>(buffer)
    }

    let toHost (clArray: ClArray<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> clArray.Length
        context.CommandQueue.Post <| Msg.CreateToHostMsg(clArray.Buffer, array)
        return array
    }

    // let copy (clArray: ClArray<'a>) = ()

    let alloc<'a when 'a : struct> (size: int) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.Context, Size size)
        return new ClArray<'a>(buffer)
    }

module ClCell =
    let toDevice (value: 'a) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.Context, Size 1)
        context.CommandQueue.Post <| Msg.CreateToHostMsg(buffer, [| value |])
        return new ClCell<'a>(buffer)
    }

    let toHost (clCell: ClCell<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> 1
        context.CommandQueue.Post <| Msg.CreateToHostMsg(clCell.Buffer, array)
        return array.[0]
    }

    //let copy (clCell: ClCell<'a>) = ()

    let alloc<'a when 'a : struct> () = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.Context, Size 1)
        return new ClCell<'a>(buffer)
    }
