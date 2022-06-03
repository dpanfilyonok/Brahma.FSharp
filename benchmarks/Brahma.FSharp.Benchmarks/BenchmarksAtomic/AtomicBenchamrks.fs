﻿namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes

[<AbstractClass>]
type AtomicBenchamrks() =
    member this.WgSize = 256

    [<ParamsSource("GlobalWorkSizeProvider")>]
    member val GlobalWorkSize = 0 with get, set

    static member GlobalWorkSizeProvider =
        seq {
            yield 1000
            yield 100_000
        }
