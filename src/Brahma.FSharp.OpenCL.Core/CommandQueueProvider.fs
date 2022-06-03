namespace Brahma.FSharp

open Brahma.FSharp.OpenCL.Translator
open OpenCL.Net
open System
open System.Runtime.InteropServices

type CommandQueueProvider(device, context, translator: FSQuotationToOpenCLTranslator) =
    let finish queue =
        let error = Cl.Finish(queue)
        if error <> ErrorCode.Success then
            raise <| Cl.Exception error

    let handleFree (free: IFreeCrate) =
        { new IFreeCrateEvaluator with
            member this.Eval crate = crate.Source.Dispose()
        }
        |> free.Apply

    let handleToGPU queue (toGpu: IToGPUCrate) =
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

    let handleToHost queue (toHost: IToHostCrate) =
        { new IToHostCrateEvaluator with
            member this.Eval (crate: ToHost<'a>) =
                let eventID = ref Unchecked.defaultof<Event>
                let clMem = crate.Source.Memory

                let marshaler = translator.Marshaler

                if marshaler.IsBlittable typeof<'a> then
                    let error =
                        Cl.EnqueueReadBuffer(
                            queue,
                            clMem,
                            Bool.False,
                            System.IntPtr(0),
                            System.IntPtr(crate.Source.Length * crate.Source.ElementSize),
                            crate.Destination,
                            0u,
                            null,
                            eventID
                        )

                    if error <> ErrorCode.Success then
                        raise (Cl.Exception error)

                    finish queue

                else
                    // TODO source or dest length??
                    let size = crate.Destination.Length * marshaler.GetTypePacking(typeof<'a>).Size
                    let hostMem = Marshal.AllocHGlobal size
                    let error =
                        Cl.EnqueueReadBuffer(
                            queue,
                            clMem,
                            Bool.False,
                            IntPtr(0),
                            IntPtr(crate.Source.Length * marshaler.GetTypePacking(typeof<'a>).Size),
                            hostMem,
                            0u,
                            null,
                            eventID
                        )

                    if error <> ErrorCode.Success then
                        raise (Cl.Exception error)

                    finish queue

                    marshaler.ReadFromUnmanaged(hostMem, crate.Destination)
                    Marshal.FreeHGlobal(hostMem)

                match crate.ReplyChannel with
                | Some ch -> ch.Reply crate.Destination
                | None -> ()
        }
        |> toHost.Apply

    let handleRun queue (run: IRunCrate) =
        { new IRunCrateEvaluator with
            member this.Eval crate =
                let range = crate.Kernel.NDRange
                let workDim = uint32 range.Dimensions
                let eventID = ref Unchecked.defaultof<Event>
                let error = Cl.EnqueueNDRangeKernel(queue, crate.Kernel.Kernel, workDim, null,
                                                    range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)
        }
        |> run.Apply

    member this.CreateQueue() =
        let processor = MailboxProcessor.Start <| fun inbox ->
            let commandQueue =
                let error = ref Unchecked.defaultof<ErrorCode>
                let props = CommandQueueProperties.None
                let queue = Cl.CreateCommandQueue(context, device, props, error)

                if error.Value <> ErrorCode.Success then
                    raise <| Cl.Exception error.Value

                queue

            let mutable itIsFirstNonqueueMsg = true

            let rec loop i = async {
                let! msg = inbox.Receive()
                match msg with
                | MsgToHost crate ->
                    itIsFirstNonqueueMsg  <- true
                    handleToHost commandQueue crate

                | MsgToGPU crate ->
                    itIsFirstNonqueueMsg  <- true
                    handleToGPU commandQueue crate

                | MsgRun crate ->
                    itIsFirstNonqueueMsg  <- true
                    handleRun commandQueue crate

                | MsgFree crate ->
                    if itIsFirstNonqueueMsg then
                        finish commandQueue
                        itIsFirstNonqueueMsg  <- false
                    handleFree crate

                | MsgSetArguments setterFunc ->
                    if itIsFirstNonqueueMsg then
                        finish commandQueue
                        itIsFirstNonqueueMsg  <- false
                    setterFunc ()

                | MsgNotifyMe ch ->
                    itIsFirstNonqueueMsg  <- true
                    finish commandQueue
                    ch.Reply ()

                | MsgBarrier syncObject ->
                    itIsFirstNonqueueMsg  <- true
                    finish commandQueue
                    syncObject.ImReady()
                    while not <| syncObject.CanContinue() do ()

                return! loop 0
            }

            loop 0

        // TODO rethink error handling?
        processor.Error.AddHandler(Handler<_>(fun _ -> raise))

        processor
