namespace Brahma.FSharp

open System

// fsharplint:disable-next-line
type clarray<'a when 'a : struct> = ClArray<'a>

// fsharplint:disable-next-line
type clcell<'a when 'a : struct> = ClCell<'a>

module ClArray =
    let toDevice (array: 'a[]) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.Context, Data array, { ClMemFlags.Default  with AllocationMode = AllocationMode.UseHostPtr })
        // context.CommandQueue.Post <| Msg.CreateToHostMsg(buffer, array)
        return new ClArray<'a>(buffer)
    }

    let toHost (clArray: ClArray<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> clArray.Length
        context.CommandQueue.Post <| Msg.CreateToHostMsg(clArray.Buffer, array)

        return array
    }

    // TODO impl it
    let copy (clArray: ClArray<'a>) = opencl { return clArray }

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

    let copy (clCell: ClCell<'a>) = opencl { return clCell }

    let alloc<'a when 'a : struct> () = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.Context, Size 1)
        return new ClCell<'a>(buffer)
    }
