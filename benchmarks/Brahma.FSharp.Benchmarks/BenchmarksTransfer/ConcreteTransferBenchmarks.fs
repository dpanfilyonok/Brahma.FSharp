namespace Brahma.FSharp.Benchmarks

[<Struct>]
type StructOfIntInt64 =
    val mutable X: int
    val mutable Y: int64
    new(x, y) = { X = x; Y = y }

[<Struct>]
type GenericStruct<'a, 'b> =
    val mutable X: 'a
    val mutable Y: 'b
    new(x, y) = { X = x; Y = y }

[<Struct>]
type GenericRecord<'a, 'b> =
    {
        mutable X: 'a
        mutable Y: 'b
    }

module Concrete =
    // Brahma
    type BrahmaAllocIntBenchmark() = inherit BrahmaAllocBenchmarks<int>()
    type BrahmaAllocStructOfIntInt64Benchmark() = inherit BrahmaAllocBenchmarks<StructOfIntInt64>()
    type BrahmaAllocGenericStructOfIntInt64Benchmark() = inherit BrahmaAllocBenchmarks<GenericStruct<int, int64>>()

    type BrahmaToDeviceIntBenchmark() = inherit BrahmaToDeviceBenchmarks<int>()
    type BrahmaToDeviceStructOfIntInt64Benchmark() = inherit BrahmaToDeviceBenchmarks<StructOfIntInt64>()
    type BrahmaToDeviceGenericStructOfIntInt64Benchmark() = inherit BrahmaToDeviceBenchmarks<GenericStruct<int, int64>>()

    type BrahmaToHostIntBenchmark() = inherit BrahmaToHostBenchmarks<int>()
    type BrahmaToHostStructOfIntInt64Benchmark() = inherit BrahmaToHostBenchmarks<StructOfIntInt64>()
    type BrahmaToHostGenericStructOfIntInt64Benchmark() = inherit BrahmaToHostBenchmarks<GenericStruct<int, int64>>()

    // Ilgpu
    type IlgpuAllocIntBenchmark() = inherit IlgpuAllocBenchmarks<int>()
    type IlgpuAllocStructOfIntInt64Benchmark() = inherit IlgpuAllocBenchmarks<StructOfIntInt64>()
    type IlgpuAllocGenericStructOfIntInt64Benchmark() = inherit IlgpuAllocBenchmarks<GenericStruct<int, int64>>()


