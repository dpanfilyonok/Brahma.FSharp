namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Brahma.OpenCL

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator

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
    member this.Free() = 
        //printfn "Free: %A" this.Length
        this.Buffer.Dispose()
    //override this.Finalize() = this.Buffer.Dispose()

type GpuKernel<'TRange, 'a, 't when 'TRange :> Brahma.OpenCL.INDRangeDimension>(device, context, srcLambda: Expr<'TRange ->'a>) =

    let kernelName = "brahmaKernel"

    let clCode =
        let translatorOptions = []
        let codeGenerator = new Translator.FSQuotationToOpenCLTranslator()
        let ast, newLambda = codeGenerator.Translate srcLambda translatorOptions
        let code = Printer.AST.Print ast
        //printfn "Code = %A" code
        code
    let compileQuery translatorOptions additionalSources =

        let program, error =
            let sources = additionalSources @ [clCode] |> List.toArray
            Cl.CreateProgramWithSource(context, uint32 (sources.Length), sources, null)

        if error <> ErrorCode.Success
        then failwithf "Program creation failed: %A" error

        let error = Cl.BuildProgram(program, 1u, [|device|], "", null, System.IntPtr.Zero)

        if error <> ErrorCode.Success
        then failwithf "Program compilation failed: %A" error

        program

    let createKernel program =
        let clKernel,error = Cl.CreateKernel(program, kernelName)
        if error <> ErrorCode.Success
        then failwithf "OpenCL kernel creation problem. Error: %A" error
        clKernel

    let additionalSources = []

    let kernel =
        let program = compileQuery [] additionalSources
        let clKernel = createKernel program
        clKernel

    let toIMem a =
        match box a with
        | :? Brahma.IMem as mem ->
           mem.Size, mem.Data
        | :? int as i -> System.IntPtr(System.Runtime.InteropServices.Marshal.SizeOf(i)),
                         box i
        | x -> failwithf "Unexpected argument: %A" x

    let setupArgument index arg =
        let argSize, argVal = toIMem arg
        let error =
                Cl.SetKernelArg(kernel, (uint32 index)
                , argSize
                , argVal)
        if error <> ErrorCode.Success
        then raise (new CLException(error))

    let range = ref Unchecked.defaultof<'TRange>

    let usedBuffers = ref [||]

    member this.SetArguments =
        let args = ref [||]
        let getStarterFunction qExpr =
            let rec go expr vars =
                match expr with
                | Patterns.Lambda (v, body) ->
                    let v =
                        if v.Type.IsArray
                        then
                            let vName = v.Name
                            let vType = typedefof<GpuArray<_>>.MakeGenericType(v.Type.GetElementType())
                            Var(vName, vType, false)
                        else v
                    Expr.Lambda(v, go body (v::vars))
                | e ->
                    let arr =
                        let allArgs =
                            let expr (v:Var) =
                                if v.Type.Name.Contains "GpuArray"
                                then
                                    let propertyInfo = v.Type.GetProperty("Buffer")
                                    Expr.PropertyGet(Expr.Var(v), propertyInfo)
                                else Expr.Var(v)

                            Expr.NewArray(
                                    typeof<obj>,
                                    vars |> List.rev |> List.map (fun v -> Expr.Coerce ((expr v), typeof<obj>))
                                    )
                            (*Expr.NewArray(
                                    typeof<obj>,
                                    vars
                                    |> List.choose 
                                        (fun v ->
                                            if v.Type.Name.Contains "GpuArray" 
                                            then Some (Expr.Coerce (Expr.Var(v), typeof<obj>))
                                            else None
                                        )
                                    )*)
                        <@@
                            let x = %%allArgs |> List.ofArray
                            range := (box x.Head) :?> 'TRange
                            args := x.Tail |> Array.ofList
                            //let b =  %%buffers
                            //usedBuffers := b 
                            !args |> Array.iteri (fun i x -> setupArgument i x)
                        @@>
                    arr
            let res =
                let e:(Expr<'TRange -> 't>) = Expr.Cast (go qExpr [])
                <@ %e @>.Compile()

            res
        getStarterFunction srcLambda

    member this.ClKernel = kernel
    member this.Range = !range :> Brahma.OpenCL.INDRangeDimension
    member this.ReleaseAllBuffers () = usedBuffers := [||]

type Free<'t>(src:GpuArray<'t>, ?replyChannel:AsyncReplyChannel<unit>) =
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToHost<'t>(src:GpuArray<'t>, dst: array<'t>, ?replyChannel:AsyncReplyChannel<array<'t>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToGPU<'t>(src:array<'t>, dst: GpuArray<'t>, ?replyChannel:AsyncReplyChannel<array<'t>>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type Run<'TRange,'a, 't when 'TRange :> Brahma.OpenCL.INDRangeDimension>
        (kernel:GpuKernel<'TRange,'a, 't>, ?replyChannel:AsyncReplyChannel<bool>) =
    member this.Kernel = kernel
    member this.ReplyChannel = replyChannel

type RunCrate =
    abstract member Apply<'ret> : RunCrateEvaluator<'ret> -> 'ret

and RunCrateEvaluator<'ret> =
    abstract member Eval<'TRange, 'a, 't when 'TRange :> Brahma.OpenCL.INDRangeDimension> : Run<'TRange, 'a, 't> -> 'ret

type ToHostCrate =
    abstract member Apply<'ret> : ToHostCrateEvaluator<'ret> -> 'ret

and ToHostCrateEvaluator<'ret> =
    abstract member Eval<'a> : ToHost<'a> -> 'ret

type ToGPUCrate =
    abstract member Apply<'ret> : ToGPUCrateEvaluator<'ret> -> 'ret

and ToGPUCrateEvaluator<'ret> =
    abstract member Eval<'a> : ToGPU<'a> -> 'ret

type FreeCrate =
    abstract member Apply<'ret> : FreeCrateEvaluator<'ret> -> 'ret

and FreeCrateEvaluator<'ret> =
    abstract member Eval<'a> : Free<'a> -> 'ret


type SyncObject (numToWait) =
    let mutable canContinue = false

    let mutable counter = 0

    member this.ImReady () =
        lock this (fun () ->
                       printfn "%A %A" counter numToWait
                       counter <- counter + 1
                       if counter = numToWait
                       then canContinue <- true)

    member this.CanContinue () = canContinue

type Msg =
    | MsgToHost of ToHostCrate
    | MsgToGPU of ToGPUCrate
    | MsgRun of RunCrate
    | MsgFree of FreeCrate
    | MsgSetArguments of (unit -> unit)
    | MsgNotifyMe of AsyncReplyChannel<unit>
    | MsgBarrier of SyncObject

    static member CreateToHostMsg m =
        {
            new ToHostCrate with
                member this.Apply e = e.Eval m
        }
        |> MsgToHost

    static member CreateToGPUMsg<'t>(src,dst) =
        {
            new ToGPUCrate with
                member this.Apply e = e.Eval (ToGPU<'t>(src,dst))
        }
        |> MsgToGPU

    static member CreateFreeMsg<'t>(src) =
        {
            new FreeCrate with
                member this.Apply e = e.Eval (Free<'t>(src))
        }
        |> MsgFree


    static member CreateRunMsg m =
        {
            new RunCrate with
                member this.Apply e = e.Eval m
        }
        |> MsgRun

    static member CreateBarrierMessages numOfQueuesOnBarrier =
        let s = new SyncObject(numOfQueuesOnBarrier)
        Array.init numOfQueuesOnBarrier (fun i -> MsgBarrier s)

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

    member this.Allocate<'t> (length:int) =
        //printfn "Allocation : %A" length
        let buf = new Brahma.OpenCL.Buffer<_>(clContext, Brahma.OpenCL.Operations.ReadWrite, true, length)
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
