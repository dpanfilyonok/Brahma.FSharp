namespace Brahma.FSharp.OpenCL

open System

type INDRangeDimension =
    abstract member GlobalWorkSize: IntPtr[] with get
    abstract member LocalWorkSize: IntPtr[] with get
    abstract member Dimensions: int

[<Struct>]
type Range1D(globalWorkSize: int, localWorkSize: int) =
    new(globalWorkSize) = Range1D(globalWorkSize, 1)

    member this.GlobalID0 = globalWorkSize

    member this.LocalID0 = localWorkSize

    interface INDRangeDimension with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSize |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSize |]
        member this.Dimensions = 1

[<Struct>]
type Range2D(globalWorkSizeX: int, globalWorkSizeY: int, localWorkSizeX: int, localWorkSizeY: int) =
    new(globalWorkSizeX, globalWorkSizeY) = Range2D(globalWorkSizeX, globalWorkSizeY, 1, 1)

    member this.GlobalID0 = globalWorkSizeX

    member this.GlobalID1 = globalWorkSizeY

    member this.LocalID0 = localWorkSizeX

    member this.LocalID1 = localWorkSizeY

    interface INDRangeDimension with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY |]
        member this.Dimensions = 2

[<Struct>]
type Range3D(globalWorkSizeX: int, globalWorkSizeY: int, globalWorkSizeZ: int, localWorkSizeX: int, localWorkSizeY: int, localWorkSizeZ: int) =
    new(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ) = Range3D(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ, 1, 1, 1)

    member this.GlobalID0 = globalWorkSizeX

    member this.GlobalID1 = globalWorkSizeY

    member this.GlobalID2 = globalWorkSizeZ

    member this.LocalID0 = localWorkSizeX

    member this.LocalID1 = localWorkSizeY

    member this.LocalID2 = localWorkSizeZ

    interface INDRangeDimension with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY; IntPtr globalWorkSizeZ |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY; IntPtr globalWorkSizeZ |]
        member this.Dimensions = 3
