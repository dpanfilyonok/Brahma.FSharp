namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

type ComputeProvider(device: Device) as this =
    let clContext =
        let error = ref Unchecked.defaultof<ErrorCode>
        let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

        if !error <> ErrorCode.Success then
            raise <| Cl.Exception !error

        ctx

    let finish queue =
        let error = Cl.Finish(queue)
        if error <> ErrorCode.Success then
            raise <| Cl.Exception error

    let getNewProcessor () = MailboxProcessor.Start <| fun inbox ->
        let commandQueue =
            let error = ref Unchecked.defaultof<ErrorCode>
            let props = CommandQueueProperties.None
            let queue = Cl.CreateCommandQueue(clContext, device, props, error)

            if !error <> ErrorCode.Success then
                raise <| Cl.Exception !error

            queue

        let mutable itIsFirstNonqueueMsg = true

        printfn "MB is started"

        let rec loop i = async {
            let! msg = inbox.Receive()
            match msg with
            | MsgToHost a ->
                //printfn "ToHost"
                itIsFirstNonqueueMsg  <- true
                this.HandleToHost(commandQueue, a)

            | MsgToGPU a ->
                //printfn "ToGPU"
                itIsFirstNonqueueMsg  <- true
                this.HandleToGPU(commandQueue, a)

            | MsgRun a ->
                //printfn "Run"
                itIsFirstNonqueueMsg  <- true
                this.HandleRun(commandQueue, a)

            | MsgFree a ->
                //printfn "Free"
                if itIsFirstNonqueueMsg then
                    finish commandQueue
                    itIsFirstNonqueueMsg  <- false
                this.HandleFree a

            | MsgSetArguments a ->
                //printfn "SetArgs"
                if itIsFirstNonqueueMsg then
                    finish commandQueue
                    itIsFirstNonqueueMsg  <- false
                a ()

            | MsgNotifyMe ch ->
                //printfn "Notify"
                itIsFirstNonqueueMsg  <- true
                finish commandQueue
                ch.Reply ()

            | MsgBarrier o ->
                //printfn "Barrier"
                itIsFirstNonqueueMsg  <- true
                finish commandQueue
                o.ImReady()
                while not <| o.CanContinue() do ()

            | MsgFinish c ->
                OpenCL.Net.Cl.Finish(commandQueue) |> ignore
                c.Reply()

            return! loop 0
        }

        loop 0

    member val CommandQueue = getNewProcessor () with get

    member val Translator = FSQuotationToOpenCLTranslator() with get

    member this.ClDevice = device

    member this.ClContext = clContext

    member this.CreateKernel(srcLambda: Expr<'a -> 'b>) = ClKernel<_,_>(device, clContext, srcLambda)

    member private this.HandleFree(free: IFreeCrate) =
        { new IFreeCrateEvaluator with
            member this.Eval crate = crate.Source.Free()
        }
        |> free.Apply

    member private this.HandleToGPU(queue, toGpu: IToGPUCrate) =
        { new IToGPUCrateEvaluator with
            member this.Eval crate =
                let eventID = ref Unchecked.defaultof<Event>

                let mem = crate.Destination.ClMemory
                let elementSize = crate.Destination.ElementSize
                let error = Cl.EnqueueWriteBuffer(queue, mem, Bool.False, IntPtr(0),
                                                  IntPtr(crate.Destination.Length * elementSize), crate.Source, 0u, null, eventID)

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)
        }
        |> toGpu.Apply

    member private this.HandleToHost(queue, toHost: IToHostCrate) =
        { new IToHostCrateEvaluator with
            member this.Eval crate =
                let eventID = ref Unchecked.defaultof<Event>
                let mem = crate.Source.ClMemory
                let elementSize = crate.Source.ElementSize
                let error = Cl.EnqueueReadBuffer(queue, mem, Bool.False, IntPtr(0),
                                                 IntPtr(crate.Source.Length * elementSize), crate.Destination, 0u, null, eventID)

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)

                finish queue

                match crate.ReplyChannel with
                | Some ch -> ch.Reply crate.Destination
                | None -> ()
        }
        |> toHost.Apply

    member private this.HandleRun(queue, run: IRunCrate) =
        { new IRunCrateEvaluator with
            member this.Eval crate =
                let range = crate.Kernel.Range
                let workDim = uint32 range.Dimensions
                let eventID = ref Unchecked.defaultof<Event>
                let error = Cl.EnqueueNDRangeKernel(queue, crate.Kernel.ClKernel, workDim, null,
                                                    range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)
        }
        |> run.Apply

    member this.GetNewProcessor () = getNewProcessor ()

    override this.ToString() =
        let mutable e = ErrorCode.Unknown
        let deviceName = Cl.GetDeviceInfo(this.ClDevice, DeviceInfo.Name, &e).ToString()
        if deviceName.Length < 20 then
            sprintf "%s" deviceName
        else
            let platform = Cl.GetDeviceInfo(this.ClDevice, DeviceInfo.Platform, &e).CastTo<Platform>()
            let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, &e).ToString()
            let deviceType =
                match Cl.GetDeviceInfo(this.ClDevice, DeviceInfo.Type, &e).CastTo<DeviceType>() with
                | DeviceType.Cpu -> "CPU"
                | DeviceType.Gpu -> "GPU"
                | DeviceType.Accelerator -> "Accelerator"
                | _ -> "another"

            sprintf "%s, %s" platformName deviceType
