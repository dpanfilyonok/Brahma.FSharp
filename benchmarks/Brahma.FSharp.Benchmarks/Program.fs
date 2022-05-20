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
open ILGPU.Runtime
open ILGPU.Runtime.Cuda
open Perfolizer.Mathematics.OutlierDetection

[<EntryPoint>]
let main argv =
    let benchmarks = BenchmarkSwitcher [|
        typeof<Concrete.BrahmaAllocIntBenchmark>
        typeof<Concrete.BrahmaAllocStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaAllocGenericStructOfIntInt64Benchmark>

        typeof<Concrete.IlgpuAllocIntBenchmark>
        typeof<Concrete.IlgpuAllocStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuAllocGenericStructOfIntInt64Benchmark>


        typeof<Concrete.BrahmaToDeviceIntBenchmark>
        typeof<Concrete.BrahmaToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToDeviceGenericStructOfIntInt64Benchmark>

        typeof<Concrete.IlgpuToDeviceIntBenchmark>
        typeof<Concrete.IlgpuToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuToDeviceGenericStructOfIntInt64Benchmark>


        typeof<Concrete.BrahmaToHostIntBenchmark>
        typeof<Concrete.BrahmaToHostStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToHostGenericStructOfIntInt64Benchmark>

        typeof<Concrete.IlgpuToHostIntBenchmark>
        typeof<Concrete.IlgpuToHostStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuToHostGenericStructOfIntInt64Benchmark>

        typeof<Concrete.FsclToDeviceIntBenchmark>
        typeof<Concrete.FsclToDeviceStructOfIntInt64Benchmark>
//        typeof<Concrete.FsclToDeviceGenericStructOfIntInt64Benchmark>
        typeof<Concrete.FsclAIntBenchmark>
        typeof<Concrete.FsclAStructOfIntInt64Benchmark>
//        typeof<Concrete.FsclAGenericStructOfIntInt64Benchmark>

        typeof<BrahmaNativeAtomicBenchmarks>
        typeof<BrahmaSpinlockAtomicBenchmarks>
        typeof<IlgpuNativeAtomicBenchmarks>
        typeof<IlgpuSpinlockAtomicBenchmarks>
    |]
//
    benchmarks.Run(
        [|"--join"; (*"--filter"; "*"*) (*"--allCategories=Brahma";*) (*"--list"; "flat"*)|],
        DefaultConfig.Instance
            .AddJob(
                Job.Dry
                    .WithWarmupCount(3)
                    .WithIterationCount(10)
                    .WithInvocationCount(3)
                    .WithOutlierMode(OutlierMode.RemoveUpper)
            )
//            .AddFilter(
//                UnionFilter(
//                    DisjunctionFilter(
//                        NameFilter(fun name -> name.Contains("Brahma")),
//                        NameFilter(fun name -> name.Contains("Ilgpu"))
//                    ),
//                NameFilter(fun name ->(* not <|*) name.Contains("Atomic"))
//                )
//            )
    ) |> ignore

//    let context = Context.CreateDefault()
//    let c = context.CreateCudaAccelerator(0)
//    printfn "%A" <| c.Name
//    let ar = Array.create 10 10
////    let worksize = new WorkSize(1024L, 64L)
//    let a = <@ Array.map(fun a -> ()) ar @>
//    let compiler = new Compiler()
//    let compResult = compiler.Compile(a)
////    compResult
////    printfn "%A" <| a.Run()
//    let result = compResult :?> IKernelExpression
//    let code = (result.KFGRoot :?> KFGKernelNode).Module.Code
//    printfn "%A" code

//    let a = Array.zeroCreate<float32> 2.0f
//    let result =
//        <@
//            a |>
//            Array.groupBy (fun a -> a % 5.0f) |>
//            // A collection composition
//            Array.map (fun (key, data) ->
//                // A sub-kernel
//                id data |>
//                // A lambda not accessing data
//                fun output ->
//                ())
//        @>.Run()

    0

