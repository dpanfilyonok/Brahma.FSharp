namespace Brahma.FSharp.OpenCL

type GpuArray<'t> (buffer:Brahma.OpenCL.Buffer<'t>, length: int) =
    member this.Buffer = buffer
    member this.Length = length
    member this.Free() = 
        //printfn "Free: %A" this.Length
        this.Buffer.Dispose()
    interface System.IDisposable with
        member this.Dispose() = this.Free()
