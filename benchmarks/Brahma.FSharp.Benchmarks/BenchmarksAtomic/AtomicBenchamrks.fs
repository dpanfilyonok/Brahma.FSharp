namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes

[<AbstractClass>]
type AtomicBenchamrks() =
    member this.WgSize = 256

    [<ParamsSource("GlobalWorkSizeProvider")>]
    member val GlobalWorkSize = 0 with get, set

    // TODO это бы тоже в настройки вынести
    static member GlobalWorkSizeProvider =
//        let base' = 10
//        let count = 3
//        Seq.init count (fun i -> base' * int (10. ** float i))

        seq {
            yield 1000
            yield 100_000
        }
