open System
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Filters
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open Brahma.FSharp.Benchmarks

open FSCL
open FSCL.Runtime
open FSCL.Compiler
open FSCL.Language

open ILGPU
open ILGPU.Backends.IL
open ILGPU.Backends.PTX
open ILGPU.IR
open ILGPU.Runtime
open ILGPU.Runtime.Cuda
open Perfolizer.Mathematics.OutlierDetection

open ILGPU
open ILGPU.AtomicOperations
open ILGPU.Runtime
open ILGPU.Runtime.Cuda

[<EntryPoint>]
let main argv =
    let benchmarks = BenchmarkSwitcher [|
        //
        typeof<Concrete.BrahmaAllocIntBenchmark>
        typeof<Concrete.BrahmaAllocStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaAllocValueOptionOfIntBenchmark>
        typeof<Concrete.BrahmaAllocBoolBenchmark>

        typeof<Concrete.IlgpuAllocIntBenchmark>
        typeof<Concrete.IlgpuAllocStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuAllocValueOptionOfIntBenchmark>

        //
        typeof<Concrete.BrahmaToDeviceIntBenchmark>
        typeof<Concrete.BrahmaToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToDeviceValueOptionOfIntBenchmark>
        typeof<Concrete.BrahmaToDeviceBoolBenchmark>

        typeof<Concrete.IlgpuToDeviceIntBenchmark>
        typeof<Concrete.IlgpuToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuToDeviceValueOptionOfIntBenchmark>

        typeof<Concrete.FsclToDeviceIntBenchmark>
        typeof<Concrete.FsclToDeviceStructOfIntInt64Benchmark>

        //
        typeof<Concrete.BrahmaToHostIntBenchmark>
        typeof<Concrete.BrahmaToHostStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToHostValueOptionOfIntBenchmark>
        typeof<Concrete.BrahmaToHostBoolBenchmark>

        typeof<Concrete.IlgpuToHostIntBenchmark>
        typeof<Concrete.IlgpuToHostStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuToHostValueOptionOfIntBenchmark>

//        //
//        typeof<BrahmaNativeAtomicBenchmarks>
//        typeof<BrahmaSpinlockAtomicBenchmarks>
//        typeof<IlgpuNativeAtomicBenchmarks>
//        typeof<IlgpuSpinlockAtomicBenchmarks>
    |]

    benchmarks.Run(
        Array.append argv [|"--join"; "--filter"; "*" (*"--allCategories=Brahma";*) (*"--list"; "flat"*)|],
        DefaultConfig.Instance
            .AddJob(
                Job.Dry
                    .WithWarmupCount(3)
                    .WithIterationCount(10)
                    .WithInvocationCount(3)
                    .WithOutlierMode(OutlierMode.RemoveUpper)
            )
    ) |> ignore

    0

