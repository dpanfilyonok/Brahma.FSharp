namespace Brahma.FSharp

open OpenCL.Net

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator

open System
open System.Runtime.InteropServices

open OpenCL.Net
open System.Collections.Generic
open FSharp.Quotations

exception EmptyDevicesException of string

type internal ExprWrapper(e: Expr) =
    override this.GetHashCode() =
        e.ToString().GetHashCode()

    override this.Equals(other: obj) =
        (not << isNull) other &&
        this.GetType() = other.GetType() &&
        e.ToString() = (other :?> ExprWrapper).ToString()

    override this.ToString() =
        e.ToString()

[<RequireQualifiedAccess>]
type ClPlatform =
    | Intel
    | AMD
    | NVIDIA
    | Pattern of string

[<RequireQualifiedAccess>]
type ClDeviceType =
    | CPU
    | GPU
    | Default

type ClContext(device: Device) as this =
    let clContext =
        let error = ref Unchecked.defaultof<ErrorCode>
        let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

        if !error <> ErrorCode.Success then
            raise (Cl.Exception !error)

        ctx

    // let a =
    //     this.GetNewProcessor()

    let tryReplay (chOpt: AsyncReplyChannel<_> option) resp queue =
        match chOpt with
        | Some ch ->
            match queue with
            | Some queue ->
                let error = OpenCL.Net.Cl.Finish(queue)
                if error <> ErrorCode.Success then
                    let e = Cl.Exception error :> Exception
                    ch.Reply <| Error e
                    raise e
                else
                    ch.Reply resp
            | None -> ch.Reply resp
        | None -> ()

    new(?platform: ClPlatform, ?deviceType: ClDeviceType) =
        let platform = defaultArg platform (ClPlatform.Pattern "*")
        let deviceType = defaultArg deviceType ClDeviceType.Default
        let device = (Device.getDevices "" DeviceType.Gpu).[0]
        ClContext device

    member this.Device = device
    member this.Context = clContext

    member val IsCachingEnabled = false with get, set
    member val internal CompilingCache = Dictionary<ExprWrapper, obj * obj * obj>() with get

    member val CommandQueue = this.GetNewProcessor() with get

    override this.ToString() =
        let mutable e = ErrorCode.Unknown
        let deviceName = Cl.GetDeviceInfo(this.Device, DeviceInfo.Name, &e).ToString()
        if deviceName.Length < 20 then
            sprintf "%s" deviceName
        else
            let platform = Cl.GetDeviceInfo(this.Device, DeviceInfo.Platform, &e).CastTo<Platform>()
            let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, &e).ToString()
            let deviceType =
                match Cl.GetDeviceInfo(this.Device, DeviceInfo.Type, &e).CastTo<DeviceType>() with
                | DeviceType.Cpu -> "CPU"
                | DeviceType.Gpu -> "GPU"
                | DeviceType.Accelerator -> "Accelerator"
                | _ -> "another"

            sprintf "%s, %s" platformName deviceType

    member this.CreateKernel srcLambda =
        new GpuKernel<_,_,_>(device, clContext, srcLambda)

    member private this.HandleFree(free: FreeCrate) =
        { new FreeCrateEvaluator<int> with
            member this.Eval (a:Free<'t>) =
                try
                    a.Source.Free()
                    tryReplay a.ReplyChannel (Ok ()) None
                with
                | e ->
                    tryReplay a.ReplyChannel (Error e) None
                    raise e
                0
        }
        |> free.Apply

    member private this.HandleToGPU(queue, toGpu: ToGPUCrate) =
        { new ToGPUCrateEvaluator<int> with
            member this.Eval a =
                let write (src:array<'t>) (dst:Buffer<'t>) =
                    let eventID = ref Unchecked.defaultof<Event>

                    let mem = dst.ClMemory
                    let elementSize = dst.ElementSize
                    let error = Cl.EnqueueWriteBuffer(queue, mem, Bool.False, System.IntPtr(0),
                                                      System.IntPtr(dst.Length * elementSize), src, 0u, null, eventID);

                    //printfn "%A" (Cl.Exception error)
                    if error <> ErrorCode.Success then
                        try
                            let e = (Cl.Exception error) :> Exception
                            tryReplay a.ReplyChannel (Error e) (Some queue)
                            raise e
                        with
                        | e -> printfn "%A" e
                    else tryReplay a.ReplyChannel (Ok ()) (Some queue)

                write a.Source a.Destination
                0
        }
        |> toGpu.Apply

    member private this.HandleToHost (queue, toHost:ToHostCrate) =
        { new ToHostCrateEvaluator<int> with
            member this.Eval a =
                let read (src:Buffer<'t>) (dst:array<'t>)=
                    let eventID = ref Unchecked.defaultof<Event>
                    let mem = src.ClMemory
                    let elementSize = src.ElementSize
                    let error = Cl.EnqueueReadBuffer(queue, mem, Bool.False, System.IntPtr(0),
                                                     System.IntPtr(src.Length * elementSize), dst, 0u, null, eventID)

                    //printfn "%A" (Cl.Exception error)
                    if error <> ErrorCode.Success
                    then tryReplay a.ReplyChannel (Error ((Cl.Exception error):> Exception)) (Some queue)
                    dst

                let res = read a.Source a.Destination
                tryReplay a.ReplyChannel (Ok res) (Some queue)
                0
        }
        |> toHost.Apply

    member private this.HandleRun (queue, run:RunCrate) =
        { new RunCrateEvaluator<int> with
            member this.Eval (a) =
                let range = a.Kernel.Range
                let workDim = uint32 range.Dimensions
                let eventID = ref Unchecked.defaultof<Event>
                let error =
                    Cl.EnqueueNDRangeKernel(queue, a.Kernel.ClKernel, workDim, null,
                                            range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)

                if error <> ErrorCode.Success
                then
                    try
                        let e = (Cl.Exception error) :> Exception
                        tryReplay a.ReplyChannel (Error e) (Some queue)
                        raise e
                    with
                    | e -> printfn "%A" e
                else tryReplay a.ReplyChannel (Ok()) (Some queue)

                0
        }
        |> run.Apply

    member this.GetNewProcessor() = MailboxProcessor.Start <| fun inbox ->
        let commandQueue =
            let error = ref Unchecked.defaultof<ErrorCode>
            let props = CommandQueueProperties.None
            let queue = Cl.CreateCommandQueue(clContext, device, props, error)

            if !error <> ErrorCode.Success then
                raise (Cl.Exception !error)

            queue

        let mutable itIsFirstNonqueueMsg = true

        printfn "MB is started"

        let rec loop i = async {
            let! msg = inbox.Receive()
            match msg with
            | MsgToHost a ->
                //printfn "ToHost"
                itIsFirstNonqueueMsg  <- true
                this.HandleToHost(commandQueue, a) |> ignore

            | MsgToGPU a ->
                //printfn "ToGPU"
                itIsFirstNonqueueMsg  <- true
                this.HandleToGPU(commandQueue, a) |> ignore

            | MsgRun a ->
                //printfn "Run"
                itIsFirstNonqueueMsg  <- true
                this.HandleRun(commandQueue, a) |> ignore

            | MsgFree a ->
                //printfn "Free"
                if itIsFirstNonqueueMsg then
                    OpenCL.Net.Cl.Finish(commandQueue) |> ignore
                    itIsFirstNonqueueMsg  <- false
                this.HandleFree a |> ignore

            | MsgSetArguments a ->
                try
                    //printfn "SetArgs"
                    if itIsFirstNonqueueMsg then
                        OpenCL.Net.Cl.Finish(commandQueue) |> ignore
                        itIsFirstNonqueueMsg  <- false
                    a ()
                with
                | e -> printfn "%A" e

            | MsgNotifyMe ch ->
                //printfn "Notify"
                itIsFirstNonqueueMsg  <- true
                OpenCL.Net.Cl.Finish(commandQueue) |> ignore
                ch.Reply ()

            | MsgBarrier o ->
                //printfn "Barrier"
                itIsFirstNonqueueMsg  <- true
                OpenCL.Net.Cl.Finish(commandQueue) |> ignore
                o.ImReady()
                while not <| o.CanContinue() do ()

            return! loop 0
        }

        loop 0

