namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open System.Runtime.InteropServices

type CommandQueueProvider =
    static member CreateQueue(context: Context, device: Device) =
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
                    CommandQueueProvider.HandleToHost(commandQueue, a)

                | MsgToGPU a ->
                    // printfn "ToGPU %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    CommandQueueProvider.HandleToGPU(commandQueue, a)

                | MsgRun a ->
                    // printfn "Run %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    CommandQueueProvider.HandleRun(commandQueue, a)

                | MsgFree a ->
                    // printfn "Free %A" <| this.GetHashCode()
                    if itIsFirstNonqueueMsg then
                        CommandQueueProvider.Finish commandQueue
                        itIsFirstNonqueueMsg  <- false
                    CommandQueueProvider.HandleFree a

                | MsgSetArguments a ->
                    // printfn "SetArgs %A" <| this.GetHashCode()
                    if itIsFirstNonqueueMsg then
                        CommandQueueProvider.Finish commandQueue
                        itIsFirstNonqueueMsg  <- false
                    a ()

                | MsgNotifyMe ch ->
                    // printfn "Notify %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    CommandQueueProvider.Finish commandQueue
                    ch.Reply ()

                | MsgBarrier o ->
                    // printfn "Barrier %A" <| this.GetHashCode()
                    itIsFirstNonqueueMsg  <- true
                    CommandQueueProvider.Finish commandQueue
                    o.ImReady()
                    while not <| o.CanContinue() do ()

                return! loop 0
            }

            loop 0

        // TODO rethink error handling?
        processor.Error.AddHandler(new Handler<_>(fun _ e -> raise e))

        processor

    static member private HandleFree(free: IFreeCrate) =
        { new IFreeCrateEvaluator with
            member this.Eval crate = crate.Source.Dispose()
        }
        |> free.Apply

    static member private HandleToGPU(queue, toGpu: IToGPUCrate) =
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

    static member private HandleToHost(queue, toHost: IToHostCrate) =
        { new IToHostCrateEvaluator with
            member this.Eval (crate: ToHost<'a>) =
                let eventID = ref Unchecked.defaultof<Event>
                let clMem = crate.Source.Memory

                let marshaler = CustomMarshaler<'a>()

                // TODO source or dest length??
                let size = crate.Destination.Length * marshaler.ElementTypeSize
                let hostMem = Marshal.AllocHGlobal size

                let error = Cl.EnqueueReadBuffer(queue, clMem, Bool.False, IntPtr(0),
                                                 IntPtr(crate.Source.Length * marshaler.ElementTypeSize), hostMem, 0u, null, eventID)

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)

                CommandQueueProvider.Finish queue

                marshaler.ReadFromUnmanaged(hostMem, crate.Destination)

                match crate.ReplyChannel with
                | Some ch -> ch.Reply crate.Destination
                | None -> ()
        }
        |> toHost.Apply

    static member private HandleRun(queue, run: IRunCrate) =
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

    static member private Finish queue =
        let error = Cl.Finish(queue)
        if error <> ErrorCode.Success then
            raise <| Cl.Exception error
