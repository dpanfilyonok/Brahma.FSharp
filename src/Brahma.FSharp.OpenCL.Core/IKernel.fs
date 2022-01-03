namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type IKernel<'TRange, 'a when 'TRange :> INDRange> =
    abstract Kernel : Kernel
    abstract NDRange : INDRange
    // not sure about naming
    abstract KernelFunc : ('TRange -> 'a)
    abstract ReleaseInternalBuffers : unit -> unit
