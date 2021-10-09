namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open System.Runtime.InteropServices

type IClMem =
    abstract member Size : IntPtr
    abstract member Data : obj

type IBuffer<'a> =
    inherit IClMem
    inherit IDisposable

    abstract ClMemory : IMem
    abstract Length : int
    abstract ElementSize : int
    abstract Free : unit -> unit
