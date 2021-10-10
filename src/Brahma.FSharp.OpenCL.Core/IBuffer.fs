namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System

type IClMem =
    abstract member Size : IntPtr
    abstract member Data : obj

type IBuffer<'a when 'a : struct> =
    inherit IClMem
    inherit IDisposable

    abstract ClMemory : IMem
    abstract Length : int
    abstract ElementSize : int
    abstract Free : unit -> unit
