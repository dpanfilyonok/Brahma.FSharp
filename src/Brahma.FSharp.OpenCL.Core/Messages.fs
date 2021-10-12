namespace Brahma.FSharp.OpenCL

type Free<'a when 'a: struct>(src: IBuffer<'a>) =
    member this.Source = src

type ToHost<'a when 'a: struct>(src: IBuffer<'a>, dst: 'a[], ?replyChannel: AsyncReplyChannel<'a[]>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToGPU<'a when 'a: struct>(src: 'a[], dst: IBuffer<'a>) =
    member this.Destination = dst
    member this.Source = src

type Run<'TRange, 'a when 'TRange :> INDRangeDimension>(kernel: IKernel<'TRange, 'a>) =
    member this.Kernel = kernel

type IRunCrate =
    abstract member Apply : IRunCrateEvaluator -> unit
and IRunCrateEvaluator =
    abstract member Eval : Run<'TRange, 'a> -> unit

type IToHostCrate =
    abstract member Apply : IToHostCrateEvaluator -> unit
and IToHostCrateEvaluator =
    abstract member Eval : ToHost<'a> -> unit

type IToGPUCrate =
    abstract member Apply : IToGPUCrateEvaluator -> unit
and IToGPUCrateEvaluator =
    abstract member Eval : ToGPU<'a> -> unit

type IFreeCrate =
    abstract member Apply : IFreeCrateEvaluator -> unit
and IFreeCrateEvaluator =
    abstract member Eval : Free<'a> -> unit

type SyncObject(numToWait: int) =
    let mutable canContinue = false

    let mutable counter = 0

    member this.ImReady() =
        lock this <| fun () ->
            counter <- counter + 1
            if counter = numToWait then canContinue <- true

    member this.CanContinue() = canContinue

type Msg =
    | MsgToHost of IToHostCrate
    | MsgToGPU of IToGPUCrate
    | MsgRun of IRunCrate
    | MsgFree of IFreeCrate
    | MsgSetArguments of (unit -> unit)
    | MsgNotifyMe of AsyncReplyChannel<unit>
    | MsgBarrier of SyncObject
    | MsgFinish of AsyncReplyChannel<unit>

    static member CreateToHostMsg<'a when 'a: struct>(src, dst, ?ch) =
        { new IToHostCrate with
            member this.Apply evaluator = evaluator.Eval <| ToHost<'a>(src, dst, ?replyChannel = ch)
        }
        |> MsgToHost

    static member CreateToGPUMsg<'a when 'a: struct>(src, dst) =
        { new IToGPUCrate with
            member this.Apply evaluator = evaluator.Eval <| ToGPU<'a>(src, dst)
        }
        |> MsgToGPU

    static member CreateFreeMsg<'a when 'a: struct>(src) =
        { new IFreeCrate with
            member this.Apply evaluator = evaluator.Eval <| Free<'a>(src)
        }
        |> MsgFree

    static member CreateRunMsg<'TRange, 'a when 'TRange :> INDRangeDimension>(kernel) =
        { new IRunCrate with
            member this.Apply evaluator = evaluator.Eval <| Run<'TRange, 'a>(kernel)
        }
        |> MsgRun

    static member CreateBarrierMessages(numOfQueuesOnBarrier: int) =
        let s = SyncObject numOfQueuesOnBarrier
        Array.init numOfQueuesOnBarrier (fun _ -> MsgBarrier s)
