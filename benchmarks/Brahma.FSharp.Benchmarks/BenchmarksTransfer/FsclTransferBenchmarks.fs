namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared
open FSCL
open FSCL.Runtime
open FSCL.Compiler
open FSCL.Language

type FsclToDeviceBenchmarks<'a>() =
    inherit TransferBenchmarks<'a>()

    [<Benchmark>]
    member this.WriteArrayToDevice() =
        <@
            this.HostArray
            |> Array.map (fun x -> x)
        @>.Run()
        |> ignore

type FsclABenchmarks<'a>() =
    inherit TransferBenchmarks<'a>()

    let mutable array = Unchecked.defaultof<'a[]>

    [<Benchmark>]
    member this.WriteArrayToDevice() =
        array <-
            <@
                this.HostArray
                |> Array.map (fun x -> x)
            @>.Run()
