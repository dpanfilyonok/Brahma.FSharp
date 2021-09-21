namespace Brahma.FSharp

open OpenCL.Net
open System
open System.Runtime.InteropServices

type ClArray<'a when 'a : struct>(buffer: ClBuffer<'a>) =
    member this.Length = ()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

type ClCell<'a when 'a : struct>(buffer: ClBuffer<'a>) =
    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

// когда мы создаем буффер с alloc and copy то это кал - вероятно будет гонка, непонятно,когда это произойдет и тп (как читать оттуда?)
// нужно сначала аллоцировать память а потом поставить в очередь операцию записи

module ClArray =
    let toDevice (array: 'a[]) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer(context, Data array)
        return ClArray(buffer)
    }

    let toHost (clArray: ClArray<'a>) = ()
    let copy (clArray: ClArray<'a>) = ()
    let alloc (size: int) = ()

module ClCell =
    let toDevice (cell: 'a) = ()
    let toHost (clCell: ClCell<'a>) = ()
    let copy (clCell: ClCell<'a>) = ()
    let alloc () = ()
