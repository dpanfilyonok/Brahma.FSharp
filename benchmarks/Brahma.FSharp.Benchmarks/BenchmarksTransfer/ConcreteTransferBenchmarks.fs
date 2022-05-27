namespace Brahma.FSharp.Benchmarks

module Concrete =
    // Brahma
    type BrahmaAllocIntBenchmark() = inherit BrahmaAllocBenchmarks<int>()
    type BrahmaAllocStructOfIntInt64Benchmark() = inherit BrahmaAllocBenchmarks<StructOfIntInt64>()
    type BrahmaAllocValueOptionOfIntBenchmark() = inherit BrahmaAllocBenchmarks<ValueOption<int>>()
    type BrahmaAllocBoolBenchmark() = inherit BrahmaAllocBenchmarks<bool>()

    type BrahmaToDeviceIntBenchmark() = inherit BrahmaToDeviceBenchmarks<int>()
    type BrahmaToDeviceStructOfIntInt64Benchmark() = inherit BrahmaToDeviceBenchmarks<StructOfIntInt64>()
    type BrahmaToDeviceValueOptionOfIntBenchmark() = inherit BrahmaToDeviceBenchmarks<ValueOption<int>>()
    type BrahmaToDeviceBoolBenchmark() = inherit BrahmaToDeviceBenchmarks<bool>()


    type BrahmaToHostIntBenchmark() = inherit BrahmaToHostBenchmarks<int>()
    type BrahmaToHostStructOfIntInt64Benchmark() = inherit BrahmaToHostBenchmarks<StructOfIntInt64>()
    type BrahmaToHostValueOptionOfIntBenchmark() = inherit BrahmaToHostBenchmarks<ValueOption<int>>()
    type BrahmaToHostBoolBenchmark() = inherit BrahmaToHostBenchmarks<bool>()

    // Ilgpu
    type IlgpuAllocIntBenchmark() = inherit IlgpuAllocBenchmarks<int>()
    type IlgpuAllocStructOfIntInt64Benchmark() = inherit IlgpuAllocBenchmarks<StructOfIntInt64>()
    type IlgpuAllocValueOptionOfIntBenchmark() = inherit IlgpuAllocBenchmarks<ValueOption<int>>()

    type IlgpuToDeviceIntBenchmark() = inherit IlgpuToDeviceBenchmarks<int>()
    type IlgpuToDeviceStructOfIntInt64Benchmark() = inherit IlgpuToDeviceBenchmarks<StructOfIntInt64>()
    type IlgpuToDeviceValueOptionOfIntBenchmark() = inherit IlgpuToDeviceBenchmarks<ValueOption<int>>()

    type IlgpuToHostIntBenchmark() = inherit IlgpuToHostBenchmarks<int>()
    type IlgpuToHostStructOfIntInt64Benchmark() = inherit IlgpuToHostBenchmarks<StructOfIntInt64>()
    type IlgpuToHostValueOptionOfIntBenchmark() = inherit IlgpuToHostBenchmarks<ValueOption<int>>()

    // Fscl
    type FsclToDeviceIntBenchmark() = inherit FsclToDeviceBenchmarks<int>()
    type FsclToDeviceStructOfIntInt64Benchmark() = inherit FsclToDeviceBenchmarks<StructOfIntInt64>()

