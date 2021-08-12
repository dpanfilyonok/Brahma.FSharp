namespace Brahma.FSharp.OpenCL

open OpenCL.Net
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

type ICommand = interface end

type ToHost<'t>(src:GpuArray<'t>, dst: array<'t>, ?replyChannel:AsyncReplyChannel<array<'t>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

    interface ICommand

type ToGPU<'t>(src:GpuArray<'t>, dst: array<'t>, ?replyChannel:AsyncReplyChannel<array<'t>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

    interface ICommand


type MakeKernel<'t>(f, ?replyChannel:AsyncReplyChannel<Kernel<'t>>) =
    member this.Function = f
    member this.ReplyChannel = replyChannel

    interface ICommand


type RunKernel<'t>(kernelRunFun, ?replyChannel:AsyncReplyChannel<Kernel<'t>>) =
    member this.KernelRunFunction = kernelRunFun
    member this.ReplyChannel = replyChannel

    interface ICommand


type Allocate<'t>(size, ?replyChannel:AsyncReplyChannel<GpuArray<'t>>) =
    member this.Size = size
    member this.ReplyChannel = replyChannel

    interface ICommand


type GPU(device: Device) =

    let clContext =
        let error = ref (Unchecked.defaultof<ErrorCode>)
        let ctx = Cl.CreateContext(null, 1u, [|device|], null, System.IntPtr.Zero, error)
        if (!error <> ErrorCode.Success)
        then raise (new Cl.Exception(!error))
        ctx
    member this.ClDevice = device

    member this.ClContext = clContext

    member this.CommandQueue = new Brahma.OpenCL.CommandQueue(clContext, device)

    member private this.Allocate (length:int) =
        new Brahma.OpenCL.Buffer<_>((this.ClContext:Context), Brahma.OpenCL.Operations.ReadWrite, true, length)

    member private this.Write (src:array<'t>, dst:GpuArray<'t>) =

        let eventID = ref Unchecked.defaultof<Event>

        let mem = dst.Buffer.Mem
        let elementSize = dst.Buffer.ElementSize
        let error = Cl.EnqueueWriteBuffer(this.CommandQueue.Queue, mem,
                    Bool.False, System.IntPtr(0),
                    System.IntPtr(dst.Length * elementSize), src, 0u, [||], eventID);

        if error <> ErrorCode.Success
        then raise (Cl.Exception(error))

    member private this.Read (src:GpuArray<'t>, dst:array<'t>) =
        let eventID = ref Unchecked.defaultof<Event>
        let mem = src.Buffer.Mem
        let elementSize = src.Buffer.ElementSize
        let error = Cl.EnqueueReadBuffer(this.CommandQueue.Queue, mem,
                    Bool.False, System.IntPtr(0),
                    System.IntPtr(src.Length * elementSize), dst, 0u, [||], eventID);

        if error <> ErrorCode.Success
        then raise (Cl.Exception(error))
        dst

    member this.Processor = MailboxProcessor.Start(fun inbox ->
        let rec loop i = async {
            let! msg = inbox.Receive()
            match (msg:ICommand) with
            | :? Allocate<'t> as a ->
                match a.ReplyChannel with
                | None -> ()
                | Some ch -> ch.Reply (GpuArray<'t>(this.Allocate(a.Size),a.Size))
            | :? ToHost<_> as a ->
                let res = this.Read(a.Source,a.Destination)
                match a.ReplyChannel with
                | None -> ()
                | Some ch -> ch.Reply res
            | :? ToGPU<_> as a -> this.Write(a.Destination,a.Source)
            | :? MakeKernel<_> as mk ->
                match mk.ReplyChannel with
                | None -> ()
                | Some ch -> ch.Reply (Kernel<_>("ctx", mk.Function))
            }
        loop 0)

type Host() =
    member this.Do () =
        let devices = Device.getDevices "*" DeviceType.Gpu
        let processor = (GPU(devices.[0])).Processor
        let a1 = Array.init 10 (fun i -> i + 1)
        let a2 = Array.init 20 (fun i -> float i + 2.0)
        let res1 = Array.zeroCreate 10
        let res2 = Array.zeroCreate 20

        let m1 = processor.PostAndReply(fun ch -> Allocate<_>(a1.Length, ch) :> ICommand)
        let m2 = processor.PostAndReply(fun ch -> Allocate<_>(a2.Length, ch) :> ICommand)

        processor.Post(ToGPU<_>(m1, a1))
        processor.Post(ToGPU<_>(m2, a2))
        let res1 = processor.PostAndReply(fun ch -> ToHost<_>(m1, res1, ch) :> ICommand)
        let res2 = processor.PostAndReply(fun ch -> ToHost<_>(m2, res2, ch) :> ICommand)

        let result =
            res1,res2

        result
