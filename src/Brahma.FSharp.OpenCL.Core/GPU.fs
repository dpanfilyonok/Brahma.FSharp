namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System

type GPU(device: Device) =

    let clContext =
        let error = ref (Unchecked.defaultof<ErrorCode>)
        let ctx = Cl.CreateContext(null, 1u, [|device|], null, System.IntPtr.Zero, error)
        if !error <> ErrorCode.Success
        then raise (Cl.Exception !error)
        ctx

    let createNewCommandQueue() =
        let error = ref Unchecked.defaultof<ErrorCode>
        let props = CommandQueueProperties.None
        let queue = Cl.CreateCommandQueue (clContext, device, props, error)

        if !error <> ErrorCode.Success
        then raise (Cl.Exception !error) 

        queue

    let finish queue =
        let error = Cl.Finish(queue)
        if error <> ErrorCode.Success
        then
            let e = (Cl.Exception error):> Exception         
            raise e

    member this.ClDevice = device

    member this.ClContext = clContext

    member this.CreateKernel (srcLambda) =
        new GpuKernel<_,_>(device, clContext, srcLambda)
    
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
                    new FreeCrateEvaluator
                    with member this.Eval (a:Free<'t>) = a.Source.Free()
                }

    member private this.HandleToGPU (queue, toGpu:ToGPUCrate) =
        toGpu.Apply
                {
                    new ToGPUCrateEvaluator
                    with member this.Eval (a) =
                            let eventID = ref Unchecked.defaultof<Event>

                            let mem = a.Destination.ClMemory
                            let elementSize = a.Destination.ElementSize
                            let error = Cl.EnqueueWriteBuffer(queue, mem, Bool.False, System.IntPtr(0),
                                                              System.IntPtr(a.Destination.Length * elementSize), a.Source, 0u, null, eventID)

                            if error <> ErrorCode.Success 
                            then raise (Cl.Exception error)
                }

    member private this.HandleToHost (queue, toHost:ToHostCrate) =
        toHost.Apply
                {
                    new ToHostCrateEvaluator
                    with member this.Eval (a) =                            
                            let eventID = ref Unchecked.defaultof<Event>
                            let mem = a.Source.ClMemory
                            let elementSize = a.Source.ElementSize
                            let error = Cl.EnqueueReadBuffer(queue, mem, Bool.False, System.IntPtr(0),
                                                             System.IntPtr(a.Source.Length * elementSize), a.Destination, 0u, null, eventID)                                
                            
                            if error <> ErrorCode.Success
                            then raise (Cl.Exception error)

                            finish queue
                                                        
                            match a.ReplyChannel with
                            | Some ch -> ch.Reply a.Destination
                            | None -> ()
                }

    member private this.HandleRun (queue, run:RunCrate) =
        run.Apply
                {
                    new RunCrateEvaluator
                    with member this.Eval (a) =
                            
                            let range = a.Kernel.Range
                            let workDim = uint32 range.Dimensions
                            let eventID = ref Unchecked.defaultof<Event>
                            let error = Cl.EnqueueNDRangeKernel(queue, a.Kernel.ClKernel, workDim, null,
                                                                range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)

                            if error <> ErrorCode.Success
                            then raise (Cl.Exception error)
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
                if itIsFirstNonqueueMsg 
                then                    
                    finish commandQueue
                    itIsFirstNonqueueMsg  <- false
                this.HandleFree a

            | MsgSetArguments a ->                
                //printfn "SetArgs" 
                if itIsFirstNonqueueMsg 
                then                    
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

            return! loop 0
            }
        loop 0)
