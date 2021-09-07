namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Brahma.OpenCL

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
                              [<Optional; DefaultParameterValue(Brahma.OpenCL.Operations.ReadWrite)>]accessMode, 
                              [<Optional; DefaultParameterValue(true)>] isHostAccesible:bool
                              ) =
        //printfn "Allocation : %A" length
        let buf = 
            
              new Brahma.OpenCL.Buffer<_>(clContext, accessMode, isHostAccesible, length)
            
        let res = new GpuArray<'t>(buf,length)
        res

    member this.Allocate<'t> (length:int, data:array<'t>,
                              [<Optional; DefaultParameterValue(Brahma.OpenCL.Operations.ReadWrite)>]accessMode, 
                              [<Optional; DefaultParameterValue(true)>] isHostAccesible:bool                              
                              ) =
        //printfn "Allocation : %A" length
        let buf = 
            if data = null || data.Length <> length 
            then 
              new Brahma.OpenCL.Buffer<_>(clContext, accessMode, isHostAccesible, length)
            else 
              new Brahma.OpenCL.Buffer<_>(clContext, accessMode, data)
        let res = new GpuArray<'t>(buf,length)
        res

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
                            let write (src:array<'t>) (dst:GpuArray<'t>) =
                                let eventID = ref Unchecked.defaultof<Event>

                                let mem = dst.Buffer.Mem
                                let elementSize = dst.Buffer.ElementSize
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
                            let read (src:GpuArray<'t>) (dst:array<'t>)=
                                let eventID = ref Unchecked.defaultof<Event>
                                let mem = src.Buffer.Mem
                                let elementSize = src.Buffer.ElementSize
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

                            //OpenCL.Net.Cl.Finish(queue)

                            //a.Kernel.ReleaseAllBuffers()

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
