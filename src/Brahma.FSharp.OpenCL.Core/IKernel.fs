namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type IKernel<'TRange, 'a when 'TRange :> INDRangeDimension> =
    abstract ArgumentsSetter : ('TRange -> 'a)
    abstract Kernel : Kernel
    abstract Range : INDRangeDimension
    abstract Code : string
    abstract ReleaseBuffers : unit -> unit
