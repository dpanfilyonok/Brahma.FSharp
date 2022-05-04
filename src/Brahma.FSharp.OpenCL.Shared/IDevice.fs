namespace Brahma.FSharp.OpenCL.Shared

[<RequireQualifiedAccess>]
type Platform =
    | Intel
    | Amd
    | Nvidia
    | Any
    | Custom of pattern: string

[<RequireQualifiedAccess>]
type DeviceType =
    | CPU
    | GPU
    | Default

type IDevice =
    abstract Name: string
    abstract Platform: Platform
    abstract DeviceType: DeviceType

    abstract MaxWorkGroupSize: int
    abstract MaxWorkItemDimensions: int
    abstract MaxWorkItemSizes: int[]

    abstract DeviceExtensions: string
