namespace Brahma.FSharp.OpenCL

open System

type INDRangeDimension =
    abstract member GlobalWorkSize: IntPtr[] with get
    abstract member LocalWorkSize: IntPtr[] with get
    abstract member Dimensions: int

type Range1D(globalWorkSize: int, localWorkSize: int) =
    new(globalWorkSize) = Range1D(globalWorkSize, 1)

    member this.GlobalID0 : int = FailIfOutsideKernel()
    member this.LocalID0 : int = FailIfOutsideKernel()

    member this.GlobalWorkSize = globalWorkSize
    member this.LocalWorkSize = localWorkSize

    interface INDRangeDimension with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSize |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSize |]
        member this.Dimensions = 1

    static member CreateValid(neededSize: int, wgSize: int) =
        let globalSzie = (neededSize + wgSize - 1) / wgSize * wgSize
        Range1D(globalSzie, wgSize)

type Range2D(globalWorkSizeX: int, globalWorkSizeY: int, localWorkSizeX: int, localWorkSizeY: int) =
    new(globalWorkSizeX, globalWorkSizeY) = Range2D(globalWorkSizeX, globalWorkSizeY, 1, 1)

    member this.GlobalID0 : int = FailIfOutsideKernel()
    member this.GlobalID1 : int = FailIfOutsideKernel()
    member this.LocalID0 : int = FailIfOutsideKernel()
    member this.LocalID1 : int = FailIfOutsideKernel()

    member this.GlobalWorkSize = (globalWorkSizeX, globalWorkSizeY)
    member this.LocalWorkSize = (localWorkSizeX, localWorkSizeY)

    interface INDRangeDimension with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY |]
        member this.Dimensions = 2

type Range3D(globalWorkSizeX: int, globalWorkSizeY: int, globalWorkSizeZ: int, localWorkSizeX: int, localWorkSizeY: int, localWorkSizeZ: int) =
    new(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ) = Range3D(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ, 1, 1, 1)

    member this.GlobalID0 : int = FailIfOutsideKernel()
    member this.GlobalID1 : int = FailIfOutsideKernel()
    member this.GlobalID2 : int = FailIfOutsideKernel()
    member this.LocalID0 : int = FailIfOutsideKernel()
    member this.LocalID1 : int = FailIfOutsideKernel()
    member this.LocalID2 : int = FailIfOutsideKernel()

    member this.GlobalWorkSize = (globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ)
    member this.LocalWorkSize = (localWorkSizeX, localWorkSizeY, localWorkSizeZ)

    interface INDRangeDimension with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY; IntPtr globalWorkSizeZ |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY; IntPtr globalWorkSizeZ |]
        member this.Dimensions = 3
