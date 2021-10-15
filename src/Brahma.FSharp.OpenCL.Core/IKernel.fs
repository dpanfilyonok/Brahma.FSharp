namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type IKernel<'TRange, 'a when 'TRange :> INDRangeDimension> =
    abstract SetArguments : ('TRange -> 'a)
    abstract Kernel : Kernel
    abstract Range : INDRangeDimension
    abstract Code : string
    abstract ReleaseAllBuffers : unit -> unit
