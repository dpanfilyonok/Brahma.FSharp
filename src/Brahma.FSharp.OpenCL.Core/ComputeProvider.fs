namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System

type ComputeProvider(context: Context, device: Device) as this =
    let finish queue =
        let error = Cl.Finish(queue)
        if error <> ErrorCode.Success then
            raise <| Cl.Exception error

    let getNewProcessor () =
        let processor = MailboxProcessor.Start <| fun inbox ->
            let commandQueue =
                let error = ref Unchecked.defaultof<ErrorCode>
                let props = CommandQueueProperties.None
                let queue = Cl.CreateCommandQueue(context, device, props, error)

                if !error <> ErrorCode.Success then
                    raise <| Cl.Exception !error

                queue

            let mutable itIsFirstNonqueueMsg = true

            // printfn "MB is started"

            let rec loop i = async {
                let! msg = inbox.Receive()
                match msg with
                | MsgToHost a ->
                    // printfn "ToHost %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    this.HandleToHost(commandQueue, a)

                | MsgToGPU a ->
                    // printfn "ToGPU %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    this.HandleToGPU(commandQueue, a)

                | MsgRun a ->
                    // printfn "Run %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    this.HandleRun(commandQueue, a)

                | MsgFree a ->
                    // printfn "Free %A" <| this.GetHashCode()
                    if itIsFirstNonqueueMsg then
                        finish commandQueue
                        itIsFirstNonqueueMsg  <- false
                    this.HandleFree a

                | MsgSetArguments a ->
                    // printfn "SetArgs %A" <| this.GetHashCode()
                    if itIsFirstNonqueueMsg then
                        finish commandQueue
                        itIsFirstNonqueueMsg  <- false
                    a ()

                | MsgNotifyMe ch ->
                    // printfn "Notify %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    finish commandQueue
                    ch.Reply ()

                | MsgBarrier o ->
                    // printfn "Barrier %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    finish commandQueue
                    o.ImReady()
                    while not <| o.CanContinue() do ()

                | MsgFinish c ->
                    // printfn "Finish %A" <| this.GetHashCode()
                    OpenCL.Net.Cl.Finish(commandQueue) |> ignore
                    c.Reply()

                return! loop 0
            }

            loop 0

        // TODO rethink error handling?
        processor.Error.AddHandler(new Handler<_>(fun _ e -> raise e))

        processor

    member val CommandQueue = getNewProcessor () with get

    member private this.HandleFree(free: IFreeCrate) =
        { new IFreeCrateEvaluator with
            member this.Eval crate = crate.Source.Free()
        }
        |> free.Apply

    member private this.HandleToGPU(queue, toGpu: IToGPUCrate) =
        { new IToGPUCrateEvaluator with
            member this.Eval crate =
                let eventID = ref Unchecked.defaultof<Event>

                let mem = crate.Destination.Memory
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
                let mem = crate.Source.Memory
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
                let error = Cl.EnqueueNDRangeKernel(queue, crate.Kernel.Kernel, workDim, null,
                                                    range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)
        }
        |> run.Apply
