namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared
open ILGPU
open ILGPU.Runtime

[<AbstractClass>]
type IlgpuTransferBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
    inherit TransferBenchmarks<'a>()

    member val DeviceArray = Unchecked.defaultof<MemoryBuffer1D<'a, Stride1D.Dense>> with get, set


    member val Accelerator =
        use context = Context.CreateDefault()
        let device = context.Devices.[0]
        device.CreateAccelerator(context)

    static member AvaliableContextsProvider =
        ClDevice.GetAvailableDevices(Platform.Nvidia)
        |> Seq.map RuntimeContext

type IlgpuAllocBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
    inherit IlgpuTransferBenchmarks<'a>()

    [<Benchmark>]
    member this.AllocArrayToDevice() =
        // TODO Точно ли оно дождется завершения?
        this.DeviceArray <- this.Accelerator.Allocate1D(this.HostArray)
        this.Accelerator.Synchronize()

        this.DeviceArray <- this.Accelerator.Allocate1D(int64 this.ArrayLength)
        this.DeviceArray.MemSetToZero()
        this.Accelerator.Synchronize()

    [<IterationCleanup>]
    member this.ClearBuffers() =
        this.DeviceArray.Dispose()

//type IlgpuToDeviceBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
//    inherit IlgpuTransferBenchmarks<'a>()
//
//    [<Benchmark>]
//    member this.WriteArrayToDevice() =
//        // TODO Точно ли оно дождется завершения?
//        this.DeviceArray <-
//            opencl {
//                let! array = ClArray.toDeviceWithFlags this.HostArray flags
//                return array
//            }
//            |> ClTask.runSync this.Context
//
//    [<IterationCleanup>]
//    member this.ClearBuffers() =
//        this.DeviceArray.Dispose()
//
//type IlgpuToHostBenchmarks<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType>() =
//    inherit IlgpuTransferBenchmarks<'a>()
//
//    // TODO мб можно в глобал сетап вынести
//    [<IterationSetup>]
//    member this.WriteArrayToDevice() =
//        this.DeviceArray <-
//            opencl {
//                let! array = ClArray.toDeviceWithFlags this.HostArray flags
//                return array
//            }
//            |> ClTask.runSync this.Context
//
//    [<Benchmark>]
//    member this.ReadArrayFromDevice() =
//        // TODO Точно ли оно дождется завершения?
//        opencl {
//            return ClArray.toHost this.DeviceArray
//        }
//        |> ClTask.runSync this.Context
//        |> ignore
//
//    [<IterationCleanup>]
//    member this.ClearBuffers() =
//        this.DeviceArray.Dispose()
