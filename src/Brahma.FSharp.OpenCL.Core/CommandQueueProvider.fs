namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open Brahma.FSharp.OpenCL.Shared
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
open FSharp.Reflection
open System.Runtime.CompilerServices

type Alligment2() =
    member val Offsets = ResizeArray<int>() with get
    member val Length = 0 with get, set

    override this.ToString() =
        sprintf "%A %i" this.Offsets this.Length

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
                let mem = crate.Source.Memory

                let roundUp n x =
                    if x % n <> 0 then
                        (x / n) * n + n
                    else
                        x

                let getElementSize (type': Type) =
                    let elementAlligment = Alligment2()
                    let rec go (type': Type) =
                        match type' with
                        | _ when type' = typeof<bool> ->
                            let size = Marshal.SizeOf typeof<BoolHostAlias>
                            let offset = roundUp size elementAlligment.Length
                            elementAlligment.Offsets.Add offset
                            elementAlligment.Length <- offset + size

                        | _ when FSharpType.IsTuple type' ->
                            FSharpType.GetTupleElements type' |> Array.iter go

                        | _ ->
                            let size = Marshal.SizeOf type'
                            let offset = roundUp size elementAlligment.Length
                            elementAlligment.Offsets.Add offset
                            elementAlligment.Length <- offset + size

                    go type'
                    elementAlligment

                let elementAlligment = getElementSize typeof<'a>
                let elementSize = elementAlligment.Length

                let size = crate.Destination.Length * elementAlligment.Length
                let mem2 = Marshal.AllocHGlobal size
                //

                let error = Cl.EnqueueReadBuffer(queue, mem, Bool.False, IntPtr(0),
                                                 IntPtr(crate.Source.Length * elementSize), mem2, 0u, null, eventID)

                // FSharpValue.Mak

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)

                CommandQueueProvider.Finish queue

                for k = 0 to crate.Destination.Length - 1 do
                    let start = IntPtr.Add(mem2, k * elementAlligment.Length)
                    let mutable i = 0
                    let rec go (type': Type) =
                        match type' with
                        // | _ when type' = typeof<bool> ->
                        //     let offset = elementAlligment.Offsets.[i]
                        //     let x = Marshal.PtrToStructure(IntPtr.Add(start, offset), type')
                        //     i <- i + 1
                        //     x

                        | _ when FSharpType.IsTuple type' ->
                            FSharpType.GetTupleElements type'
                            |> Array.map go
                            |> fun x -> FSharpValue.MakeTuple(x, type')

                        | _ ->
                            let offset = elementAlligment.Offsets.[i]
                            let x = Marshal.PtrToStructure(IntPtr.Add(start, offset), type')
                            i <- i + 1
                            x

                    crate.Destination.[i] <- unbox<'a> <| go typeof<'a>

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
