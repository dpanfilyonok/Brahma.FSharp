namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Attributes
open FsCheck

type CommonConfig() =
    inherit ManualConfig()

    do
        base.AddJob(
            Job.Dry
                .WithWarmupCount(3)
                .WithIterationCount(10)
                .WithInvocationCount(3)
        ) |> ignore

[<Config(typeof<CommonConfig>)>]
[<AbstractClass>]
type TransferBenchmarks<'a>() =
    member val HostArray = Unchecked.defaultof<'a[]> with get, set

    [<ParamsSource("ArrayLengthProvider")>]
    member val ArrayLength = 0 with get, set

    [<GlobalSetup>]
    member this.InitializeHostArray() =
        this.HostArray <-
            Arb.generate<'a>
            |> Gen.sample 0 this.ArrayLength
            |> Array.ofList

    // TODO это бы тоже в настройки вынести
    static member ArrayLengthProvider =
        let base' = 100
        let count = 5
        Seq.init count (fun i -> base' * int (10. ** float i))
