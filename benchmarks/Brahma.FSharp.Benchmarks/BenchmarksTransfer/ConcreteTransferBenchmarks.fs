namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes

module Concrete =
    // Brahma
    [<BenchmarkCategory("Brahma")>] type BrahmaAllocIntBenchmark() = inherit BrahmaAllocBenchmarks<int>()
    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaAllocStructOfIntInt64Benchmark() = inherit BrahmaAllocBenchmarks<StructOfIntInt64>()
    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaAllocGenericStructOfIntInt64Benchmark() = inherit BrahmaAllocBenchmarks<GenericStruct<int, int64>>()

    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaToDeviceIntBenchmark() = inherit BrahmaToDeviceBenchmarks<int>()
    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaToDeviceStructOfIntInt64Benchmark() = inherit BrahmaToDeviceBenchmarks<StructOfIntInt64>()
    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaToDeviceGenericStructOfIntInt64Benchmark() = inherit BrahmaToDeviceBenchmarks<GenericStruct<int, int64>>()

    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaToHostIntBenchmark() = inherit BrahmaToHostBenchmarks<int>()
    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaToHostStructOfIntInt64Benchmark() = inherit BrahmaToHostBenchmarks<StructOfIntInt64>()
    [<BenchmarkCategory("Brahma", "Alloc", "Int")>] type BrahmaToHostGenericStructOfIntInt64Benchmark() = inherit BrahmaToHostBenchmarks<GenericStruct<int, int64>>()

    // Ilgpu
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuAllocIntBenchmark() = inherit IlgpuAllocBenchmarks<int>()
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuAllocStructOfIntInt64Benchmark() = inherit IlgpuAllocBenchmarks<StructOfIntInt64>()
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuAllocGenericStructOfIntInt64Benchmark() = inherit IlgpuAllocBenchmarks<GenericStruct<int, int64>>()

    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuToDeviceIntBenchmark() = inherit IlgpuToDeviceBenchmarks<int>()
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuToDeviceStructOfIntInt64Benchmark() = inherit IlgpuToDeviceBenchmarks<StructOfIntInt64>()
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuToDeviceGenericStructOfIntInt64Benchmark() = inherit IlgpuToDeviceBenchmarks<GenericStruct<int, int64>>()

    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuToHostIntBenchmark() = inherit IlgpuToHostBenchmarks<int>()
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuToHostStructOfIntInt64Benchmark() = inherit IlgpuToHostBenchmarks<StructOfIntInt64>()
    [<BenchmarkCategory("Ilgpu", "Alloc", "Int")>] type IlgpuToHostGenericStructOfIntInt64Benchmark() = inherit IlgpuToHostBenchmarks<GenericStruct<int, int64>>()

    // Fscl
    type FsclToDeviceIntBenchmark() = inherit FsclToDeviceBenchmarks<int>()
    type FsclToDeviceStructOfIntInt64Benchmark() = inherit FsclToDeviceBenchmarks<StructOfIntInt64>()
//    type FsclToDeviceGenericStructOfIntInt64Benchmark() = inherit FsclToDeviceBenchmarks<GenericStruct<int, int64>>()

    type FsclAIntBenchmark() = inherit FsclABenchmarks<int>()
    type FsclAStructOfIntInt64Benchmark() = inherit FsclABenchmarks<StructOfIntInt64>()
//    type FsclAGenericStructOfIntInt64Benchmark() = inherit FsclABenchmarks<int * int64>()


