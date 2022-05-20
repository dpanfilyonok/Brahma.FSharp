namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared
open FSharp.Quotations

[<AbstractClass>]
type SimpleBenchamrks() =
    member val Program = Unchecked.defaultof<ClProgram<Range1D, int clcell -> unit>> with get, set

    member val Ker = Unchecked.defaultof<ClKernel<Range1D,(clcell<int> -> unit)>> with get, set

    member val Cell = Unchecked.defaultof<int clcell> with get, set

    [<ParamsSource("AvaliableContextsProvider")>]
    member val Context = Unchecked.defaultof<RuntimeContext> with get, set

    member this.GlobalWorkSize = 100
    member this.WgSize = 256

    abstract Command: Expr<Range1D -> int clcell -> unit>

    [<GlobalSetup>]
    member this.CompileProgram() =
        this.Program <- this.Context.ClContext.Compile(this.Command)

    [<IterationSetup>]
    member this.AllocCellOnDevice() =
       this.Cell <-
            opencl {
                return! ClCell.alloc<int> ()
            }
            |> ClTask.runSync this.Context
       this.Ker <- this.Program.GetKernel()

    [<Benchmark>]
    member this.AllocArrayToDevice() =
//        opencl {
//            do! runKernel this.Program <| fun kernel ->
//                kernel
//                <| Range1D.CreateValid(this.GlobalWorkSize, this.WgSize)
//                <| this.Cell
//        }
//        |> ClTask.runSync this.Context
        this.Context.CommandQueue.Post(Msg.MsgSetArguments (fun () -> this.Ker.KernelFunc (Range1D.CreateValid(this.GlobalWorkSize, this.WgSize)) this.Cell))
//        this.Context.CommandQueue.Post(Msg.CreateRunMsg this.Ker)
        this.Context.CommandQueue.PostAndReply(MsgNotifyMe)

    [<IterationCleanup>]
    member this.CleanCell() =
        this.Cell.Dispose()

    static member AvaliableContextsProvider =
        ClDevice.GetAvailableDevices(Platform.Nvidia)
        |> Seq.map RuntimeContext

type SimpleBenchamrks1() =
    inherit SimpleBenchamrks()

    override this.Command =
        <@
            fun (range: Range1D) (acc: int clcell) ->
                acc.Value <- 1
        @>

type SimpleBenchamrks2() =
    inherit SimpleBenchamrks()

    override this.Command =
        <@
            fun (range: Range1D) (acc: int clcell) ->
                atomic (fun x -> x + 1) acc.Value |> ignore
        @>
