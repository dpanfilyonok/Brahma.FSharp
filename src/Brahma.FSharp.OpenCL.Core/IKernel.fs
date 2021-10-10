namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type IKernel<'TRange, 'a when 'TRange :> INDRangeDimension> =
    abstract SetArguments : ('TRange -> 'a)
    abstract ClKernel : Kernel
    abstract Range : INDRangeDimension
    abstract ReleaseAllBuffers : unit -> unit
    abstract ClCode : string
