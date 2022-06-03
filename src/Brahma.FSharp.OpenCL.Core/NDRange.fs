namespace Brahma.FSharp

open System

type INDRange =
    abstract member GlobalWorkSize: IntPtr[] with get
    abstract member LocalWorkSize: IntPtr[] with get
    abstract member Dimensions: int

(*
    TODO + get_num_groups (uint dimindx)
    TODO + get_group_id (uint dimindx)
*)

/// 1-dimensional index space
type Range1D(globalWorkSize: int, localWorkSize: int) =
    new(globalWorkSize) = Range1D(globalWorkSize, 1)

    member this.GlobalID0 : int = FailIfOutsideKernel()
    member this.LocalID0 : int = FailIfOutsideKernel()

    member this.GlobalWorkSize = globalWorkSize
    member this.LocalWorkSize = localWorkSize

    interface INDRange with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSize |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSize |]
        member this.Dimensions = 1

    static member CreateValid(neededSize: int, wgSize: int) =
        let globalSize = (neededSize + wgSize - 1) / wgSize * wgSize
        Range1D(globalSize, wgSize)

/// 2-dimensional index space
type Range2D(globalWorkSizeX: int, globalWorkSizeY: int, localWorkSizeX: int, localWorkSizeY: int) =
    new(globalWorkSizeX, globalWorkSizeY) = Range2D(globalWorkSizeX, globalWorkSizeY, 1, 1)

    member this.GlobalID0 : int = FailIfOutsideKernel()
    member this.GlobalID1 : int = FailIfOutsideKernel()
    member this.LocalID0 : int = FailIfOutsideKernel()
    member this.LocalID1 : int = FailIfOutsideKernel()

    member this.GlobalWorkSize = (globalWorkSizeX, globalWorkSizeY)
    member this.LocalWorkSize = (localWorkSizeX, localWorkSizeY)

    interface INDRange with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY |]
        member this.Dimensions = 2

/// 3-dimensional index space
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

    interface INDRange with
        member this.GlobalWorkSize with get () = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY; IntPtr globalWorkSizeZ |]
        member this.LocalWorkSize with get () = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY; IntPtr globalWorkSizeZ |]
        member this.Dimensions = 3
