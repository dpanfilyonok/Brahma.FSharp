namespace Brahma.FSharp.OpenCL

open OpenCL.Net

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator

open System
open System.Runtime.InteropServices

type GPU(device: Device) =

    let clContext =
        let error = ref (Unchecked.defaultof<ErrorCode>)
        let ctx = Cl.CreateContext(null, 1u, [|device|], null, System.IntPtr.Zero, error)
        if (!error <> ErrorCode.Success)
        then raise (new Cl.Exception(!error))
        ctx

    let createNewCommandQueue() =
        let error = ref Unchecked.defaultof<ErrorCode>
        let props = CommandQueueProperties.None
        let queue = Cl.CreateCommandQueue (clContext, device, props, error)

        if !error <> ErrorCode.Success
        then raise <| Cl.Exception(!error)

        queue

    member this.ClDevice = device

    member this.ClContext = clContext

    member this.CreateKernel (srcLambda) =
        new GpuKernel<_,_,_>(device, clContext, srcLambda)
    
    member this.Allocate<'t> (length:int,                               
                              ?hostAccessMode: HostAccessMode, 
                              ?allocationMode: AllocationMode, 
                              ?deviceAccessMode : DeviceAccessMode
                              ) =        
            
        new Buffer<'t>(clContext, length, ?hostAccessMode = hostAccessMode, ?allocationMode = allocationMode, ?deviceAccessMode = deviceAccessMode)

    member this.Allocate<'t> (data:array<'t>,
                              ?hostAccessMode: HostAccessMode, 
                              ?allocationMode: AllocationMode, 
                              ?deviceAccessMode : DeviceAccessMode
                              ) =
        new Buffer<'t>(clContext, data, ?hostAccessMode = hostAccessMode, ?allocationMode = allocationMode, ?deviceAccessMode = deviceAccessMode)        

    member private this.HandleFree (free:FreeCrate) =
        free.Apply
                {
                    new FreeCrateEvaluator<int>
                    with member this.Eval (a:Free<'t>) =
                            a.Source.Free()
                            0
                }

    member private this.HandleToGPU (queue, toGpu:ToGPUCrate) =
        toGpu.Apply
                {
                    new ToGPUCrateEvaluator<int>
                    with member this.Eval (a) =
                            let write (src:array<'t>) (dst:Buffer<'t>) =
                                let eventID = ref Unchecked.defaultof<Event>

                                let mem = dst.ClMemory
                                let elementSize = dst.ElementSize
                                let error = Cl.EnqueueWriteBuffer(queue, mem, Bool.False, System.IntPtr(0),
                                                                  System.IntPtr(dst.Length * elementSize), src, 0u, null, eventID);

                                if error <> ErrorCode.Success
                                then raise (Cl.Exception error)

                            write a.Source a.Destination
                            0
                }

    member private this.HandleToHost (queue, toHost:ToHostCrate) =
        toHost.Apply
                {
                    new ToHostCrateEvaluator<int>
                    with member this.Eval (a) =
                            let read (src:Buffer<'t>) (dst:array<'t>)=
                                let eventID = ref Unchecked.defaultof<Event>
                                let mem = src.ClMemory
                                let elementSize = src.ElementSize
                                let error = Cl.EnqueueReadBuffer(queue, mem, Bool.False, System.IntPtr(0),
                                                                 System.IntPtr(src.Length * elementSize), dst, 0u, null, eventID)

                                if error <> ErrorCode.Success
                                then raise (Cl.Exception(error))
                                dst
                            let res = read a.Source a.Destination
                            match a.ReplyChannel with
                            | None -> ()
                            | Some ch ->
                                OpenCL.Net.Cl.Finish(queue)
                                ch.Reply res
                            0
                }

    member private this.HandleRun (queue, run:RunCrate) =
        run.Apply
                {
                    new RunCrateEvaluator<int>
                    with member this.Eval (a) =
                            let runKernel (kernel:GpuKernel<'TRange,'a, 't>) =
                                let range = kernel.Range
                                let workDim = uint32 range.Dimensions
                                let eventID = ref Unchecked.defaultof<Event>
                                let error =
                                    Cl.EnqueueNDRangeKernel(queue, kernel.ClKernel, workDim, null,
                                                            range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)
                                
                                if error <> ErrorCode.Success
                                then
                                    printfn "Run failed: %A" (Cl.Exception error)
                                    raise (Cl.Exception error)

                            runKernel a.Kernel

                            match a.ReplyChannel with
                            | None -> ()
                            | Some ch ->
                                OpenCL.Net.Cl.Finish(queue) 
                                ch.Reply true
                            0
                }

    member this.GetNewProcessor () = MailboxProcessor.Start(fun inbox ->

        let commandQueue = createNewCommandQueue()

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
                if itIsFirstNonqueueMsg 
                then                    
                    OpenCL.Net.Cl.Finish(commandQueue)
                    itIsFirstNonqueueMsg  <- false
                this.HandleFree a

            | MsgSetArguments a ->
                //printfn "SetArgs" 
                if itIsFirstNonqueueMsg 
                then                    
                    OpenCL.Net.Cl.Finish(commandQueue)
                    itIsFirstNonqueueMsg  <- false
                a ()

            | MsgNotifyMe ch ->
                //printfn "Notify"
                itIsFirstNonqueueMsg  <- true
                OpenCL.Net.Cl.Finish(commandQueue)
                ch.Reply ()

            | MsgBarrier o ->
                //printfn "Barrier"
                itIsFirstNonqueueMsg  <- true
                OpenCL.Net.Cl.Finish(commandQueue)
                o.ImReady()
                while not <| o.CanContinue() do ()

            return! loop 0
            }
        loop 0)
