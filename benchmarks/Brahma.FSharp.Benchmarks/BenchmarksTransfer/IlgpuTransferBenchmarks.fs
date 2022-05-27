namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared
open ILGPU
open ILGPU.Runtime
open ILGPU.Runtime.Cuda

[<AbstractClass>]
type IlgpuTransferBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
    inherit TransferBenchmarks<'a>()

    member val DeviceArray = Unchecked.defaultof<MemoryBuffer1D<'a, Stride1D.Dense>> with get, set

    member val Accelerator =
        let context = Context.CreateDefault()
        context.CreateCudaAccelerator(0)

type IlgpuAllocBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
    inherit IlgpuTransferBenchmarks<'a>()

    [<Benchmark>]
    member this.AllocArrayToDevice() =
        this.DeviceArray <- this.Accelerator.Allocate1D(int64 this.ArrayLength)
        this.Accelerator.Synchronize()

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()

type IlgpuToDeviceBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
    inherit IlgpuTransferBenchmarks<'a>()

    [<Benchmark>]
    member this.WriteArrayToDevice() =
        this.DeviceArray <- this.Accelerator.Allocate1D(this.HostArray)
        this.Accelerator.Synchronize()

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()

type IlgpuToHostBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
    inherit IlgpuTransferBenchmarks<'a>()

    // TODO мб можно в глобал сетап вынести
    [<IterationSetup>]
    member this.WriteArrayToDevice() =
        this.DeviceArray <- this.Accelerator.Allocate1D(this.HostArray)
        this.Accelerator.Synchronize()

    [<Benchmark>]
    member this.ReadArrayFromDevice() =
        let target = Array.zeroCreate<'a> this.ArrayLength
        this.DeviceArray.CopyToCPU(target)
        this.Accelerator.Synchronize()

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()
