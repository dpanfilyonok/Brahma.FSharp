namespace Brahma.FSharp.OpenCL

open OpenCL.Net

// should depends of runtime context
// TODO rewrite?
type IKernel<'TRange, 'a when 'TRange :> INDRange> =
    abstract Kernel : Kernel
    abstract NDRange : INDRange
    // TODO not sure about naming
    abstract KernelFunc : ('TRange -> 'a)
    abstract ReleaseInternalBuffers : unit -> unit
