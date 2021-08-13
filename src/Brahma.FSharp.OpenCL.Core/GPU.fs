namespace Brahma.FSharp.OpenCL

open OpenCL.Net

module Device =
    open System.Text.RegularExpressions

    let private wildcardToRegex (pattern:string) =
        "^" + Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".") + "$"

    let getDevices platformName deviceType =
        let platformNameRegex = Regex(wildcardToRegex(platformName), RegexOptions.IgnoreCase);
        let error = ref (Unchecked.defaultof<ErrorCode>)

        Cl.GetPlatformIDs(error)
        |> Array.choose (fun platform ->
                if (platformNameRegex.Match(Cl.GetPlatformInfo(platform, PlatformInfo.Name, error).ToString()).Success)
                then Cl.GetDeviceIDs(platform, deviceType, error) |> Some
                else None
                )
        |> Array.concat

type GpuArray<'t> (buffer:Brahma.OpenCL.Buffer<'t>, length) =
    member this.Buffer = buffer
    member this.Length = length

type Kernel<'t>(ctx, f) =
    member this.Run () = ()

type ToHost<'t>(src:GpuArray<'t>, dst: array<'t>, ?replyChannel:AsyncReplyChannel<array<'t>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToGPU<'t>(src:array<'t>, dst: GpuArray<'t>, ?replyChannel:AsyncReplyChannel<array<'t>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel


type MakeKernel<'t>(f, ?replyChannel:AsyncReplyChannel<Kernel<'t>>) =
    member this.Function = f
    member this.ReplyChannel = replyChannel


type RunKernel<'t>(kernelRunFun, ?replyChannel:AsyncReplyChannel<Kernel<'t>>) =
    member this.KernelRunFunction = kernelRunFun
    member this.ReplyChannel = replyChannel


type Allocate<'t>(size, replyChannel:AsyncReplyChannel<GpuArray<'t>>) =
    member this.Size = size
    member this.ReplyChannel = replyChannel

type AllocateCrate =
    abstract member Apply : AllocateCrateEvaluator -> unit

and AllocateCrateEvaluator =
    abstract member Eval<'a> : Allocate<'a> -> unit

type ToHostCrate =
    abstract member Apply<'ret> : ToHostCrateEvaluator<'ret> -> 'ret

and ToHostCrateEvaluator<'ret> =
    abstract member Eval<'a> : ToHost<'a> -> 'ret

type ToGPUCrate =
    abstract member Apply<'ret> : ToGPUCrateEvaluator<'ret> -> 'ret

and ToGPUCrateEvaluator<'ret> =
    abstract member Eval<'a> : ToGPU<'a> -> 'ret

type Msg =
    | MsgAllocate of AllocateCrate
    | MsgToHost of ToHostCrate
    | MsgToGPU of ToGPUCrate

type GPU(device: Device) =

    let clContext =
        let error = ref (Unchecked.defaultof<ErrorCode>)
        let ctx = Cl.CreateContext(null, 1u, [|device|], null, System.IntPtr.Zero, error)
        if (!error <> ErrorCode.Success)
        then raise (new Cl.Exception(!error))
        ctx
    member this.ClDevice = device

    member this.ClContext = clContext
    member private this.HandleAllocate (alloc:AllocateCrate) =
            alloc.Apply
                {
                    new AllocateCrateEvaluator
                    with member __.Eval (a) =
                            let length = a.Size
                            let buf = new Brahma.OpenCL.Buffer<_>(clContext, Brahma.OpenCL.Operations.ReadWrite, true, length)
                            let res = new GpuArray<'a>(buf,a.Size)
                            a.ReplyChannel.Reply res
                }

    member private this.HandleToGPU (queue:Brahma.OpenCL.CommandQueue, toGpu:ToGPUCrate) =
        toGpu.Apply
                {
                    new ToGPUCrateEvaluator<int>
                    with member __.Eval (a) =
                            let write (src:array<'t>) (dst:GpuArray<'t>) =
                                let eventID = ref Unchecked.defaultof<Event>

                                let mem = dst.Buffer.Mem
                                let elementSize = dst.Buffer.ElementSize
                                let error = Cl.EnqueueWriteBuffer(queue.Queue, mem,
                                            Bool.False, System.IntPtr(0),
                                            System.IntPtr(dst.Length * elementSize), src, 0u, null, eventID);

                                if error <> ErrorCode.Success
                                then
                                    printfn "Error in write: %A" error
                                    raise (Cl.Exception(error))

                            write a.Source a.Destination
                            0
                }
    member private this.HandleToHost (queue:Brahma.OpenCL.CommandQueue, toHost:ToHostCrate) =
        toHost.Apply
                {
                    new ToHostCrateEvaluator<int>
                    with member __.Eval (a) =
                            let read (src:GpuArray<'t>) (dst:array<'t>)=
                                let eventID = ref Unchecked.defaultof<Event>
                                let mem = src.Buffer.Mem
                                let elementSize = src.Buffer.ElementSize
                                let error = Cl.EnqueueReadBuffer(queue.Queue, mem,
                                            Bool.False, System.IntPtr(0),
                                            System.IntPtr(src.Length * elementSize), dst, 0u, null, eventID);

                                if error <> ErrorCode.Success
                                then raise (Cl.Exception(error))
                                dst
                            let res = read a.Source a.Destination
                            match a.ReplyChannel with
                            | None -> ()
                            | Some ch -> ch.Reply res
                            0
                }
    member this.GetNewProcessor () = MailboxProcessor.Start(fun inbox ->

        let commandQueue = new Brahma.OpenCL.CommandQueue(clContext, device)

        printfn "MB is started"
        let rec loop i = async {
            printfn "In loop"
            let! msg = inbox.Receive()
            printfn "Message: %A" msg
            match msg with
            | MsgAllocate a ->
                printfn "Allocation"
                this.HandleAllocate a

            | MsgToHost a ->
                printf "ToHost"
                this.HandleToHost(commandQueue, a) |> ignore

            | MsgToGPU a ->
                printf "ToGPU"
                this.HandleToGPU(commandQueue, a) |> ignore

            return! loop 0
            }
        loop 0)

type Host() =

    let makeAllocateMsg a =
        {
            new AllocateCrate with
                member __.Apply e = e.Eval a
        }
        |> MsgAllocate

    let makeToHostMsg a =
        {
            new ToHostCrate with
                member __.Apply e = e.Eval a
        }
        |> MsgToHost

    let makeToGPUMsg a =
        {
            new ToGPUCrate with
                member __.Apply e = e.Eval a
        }
        |> MsgToGPU

    member this.Do () =
        let devices = Device.getDevices "*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        let processor1 = gpu.GetNewProcessor ()
        let processor2 = gpu.GetNewProcessor ()
        let a1 = Array.init 10 (fun i -> i + 1)
        let a2 = Array.init 20 (fun i -> float i + 2.0)
        let res1 = Array.zeroCreate 10
        let res2 = Array.zeroCreate 20

        let m1 = processor1.PostAndReply(fun ch -> makeAllocateMsg(Allocate<_>(a1.Length, ch)))
        let m2 = processor1.PostAndReply(fun ch -> makeAllocateMsg(Allocate<_>(a2.Length, ch)))


        processor1.Post(makeToGPUMsg(ToGPU<_>(a1, m1)))
        processor2.Post(makeToGPUMsg(ToGPU<_>(a2, m2)))
        //This code is unsafe because there is no synchronization between processors
        //It is just to show that we can share buffers between queues on the same device
        let res1 = processor2.PostAndReply(fun ch -> makeToHostMsg(ToHost<_>(m1, res1, ch)))
        let res2 = processor1.PostAndReply(fun ch -> makeToHostMsg(ToHost<_>(m2, res2, ch)))

        let result =
            res1,res2

        result
