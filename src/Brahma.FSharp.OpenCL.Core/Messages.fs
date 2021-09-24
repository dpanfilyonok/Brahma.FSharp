namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type Free<'t>(src:Buffer<'t>, ?replyChannel:AsyncReplyChannel<Result<unit, System.Exception>>) =
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToHost<'t>(src:Buffer<'t>, dst: array<'t>, ?replyChannel:AsyncReplyChannel<Result<array<'t>, System.Exception>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToGPU<'t>(src:array<'t>, dst: Buffer<'t>, ?replyChannel:AsyncReplyChannel<Result<unit, System.Exception>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type Run<'TRange,'a when 'TRange :> INDRangeDimension>
        (kernel:GpuKernel<'TRange,'a>, ?replyChannel:AsyncReplyChannel<Result<unit, System.Exception>>) =
    member this.Kernel = kernel
    member this.ReplyChannel = replyChannel

type RunCrate =
    abstract member Apply : RunCrateEvaluator -> unit

and RunCrateEvaluator =
    abstract member Eval<'TRange, 'a when 'TRange :> INDRangeDimension> : Run<'TRange, 'a> -> unit

type ToHostCrate =
    abstract member Apply : ToHostCrateEvaluator -> unit

and ToHostCrateEvaluator =
    abstract member Eval<'a> : ToHost<'a> -> unit

type ToGPUCrate =
    abstract member Apply : ToGPUCrateEvaluator -> unit

and ToGPUCrateEvaluator =
    abstract member Eval<'a> : ToGPU<'a> -> unit

type FreeCrate =
    abstract member Apply : FreeCrateEvaluator -> unit

and FreeCrateEvaluator =
    abstract member Eval<'a> : Free<'a> -> unit


type SyncObject (numToWait) =
    let mutable canContinue = false

    let mutable counter = 0

    member this.ImReady () =
        lock this (fun () ->
                       counter <- counter + 1
                       if counter = numToWait
                       then canContinue <- true)

    member this.CanContinue () = canContinue

type Msg =
    | MsgToHost of ToHostCrate
    | MsgToGPU of ToGPUCrate
    | MsgRun of RunCrate
    | MsgFree of FreeCrate
    | MsgSetArguments of (unit -> unit)
    | MsgNotifyMe of AsyncReplyChannel<unit>
    | MsgBarrier of SyncObject

    static member CreateToHostMsg<'t> (src, dst, ?ch) =
        {
            new ToHostCrate with
                member this.Apply e = e.Eval (ToHost<'t>(src, dst, ?replyChannel = ch))
        }
        |> MsgToHost

    static member CreateToGPUMsg<'t>(src, dst, ?ch) =
        {
            new ToGPUCrate with
                member this.Apply e = e.Eval (ToGPU<'t>(src, dst, ?replyChannel = ch))
        }
        |> MsgToGPU

    static member CreateFreeMsg(src, ?ch) =
        {
            new FreeCrate with
                member this.Apply e = e.Eval (Free(src, ?replyChannel = ch))
        }
        |> MsgFree

    static member CreateRunMsg<'TRange,'a when 'TRange :> INDRangeDimension> (kernel, ?ch) =
        {
            new RunCrate with
                member this.Apply e = e.Eval (Run<'TRange,'a>(kernel, ?replyChannel = ch)) 
        }
        |> MsgRun

    static member CreateBarrierMessages numOfQueuesOnBarrier =
        let s = SyncObject numOfQueuesOnBarrier
        Array.init numOfQueuesOnBarrier (fun i -> MsgBarrier s)
