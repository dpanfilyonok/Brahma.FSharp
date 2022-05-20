namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Filters
open BenchmarkDotNet.Jobs

type CommonConfig() =
    inherit ManualConfig()

    do
        base.AddJob(
            Job.Dry
                .WithWarmupCount(3)
                .WithIterationCount(10)
                .WithInvocationCount(3)
        ) |> ignore

//        base.AddFilter(
//            UnionFilter(
////                DisjunctionFilter(
////                    NameFilter(fun name -> name.Contains("Brahma")),
////                    NameFilter(fun name -> name.Contains("Ilgpu"))
////                ),
//                NameFilter(fun name -> not <| name.Contains("Atomic"))
//            )
//        ) |> ignore

