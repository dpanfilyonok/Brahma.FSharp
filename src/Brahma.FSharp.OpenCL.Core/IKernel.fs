namespace Brahma.FSharp

open OpenCL.Net

type IKernel =
    abstract Kernel : Kernel
    abstract NDRange : INDRange
