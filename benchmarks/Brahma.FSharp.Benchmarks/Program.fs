open BenchmarkDotNet.Running
open Brahma.FSharp.Benchmarks

[<EntryPoint>]
let main argv =
    let benchmarks = BenchmarkSwitcher [|
        typeof<Concrete.BrahmaAllocIntBenchmark>
        typeof<Concrete.BrahmaAllocStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaAllocGenericStructOfIntInt64Benchmark>

        typeof<Concrete.BrahmaToDeviceIntBenchmark>
        typeof<Concrete.BrahmaToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToDeviceGenericStructOfIntInt64Benchmark>

        typeof<Concrete.BrahmaToHostIntBenchmark>
        typeof<Concrete.BrahmaToHostStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToHostGenericStructOfIntInt64Benchmark>
    |]

    benchmarks.Run argv |> ignore
    0

