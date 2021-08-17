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

type GpuKernel<'TRange, 'a, 't when 'TRange :> Brahma.OpenCL.INDRangeDimension>(device, context, srcLambda: Expr<'TRange ->'a>) =

    let kernelName = "brahmaKernel"

    let clCode =
        let translatorOptions = []
        let codeGenerator = new Translator.FSQuotationToOpenCLTranslator()
        let ast, newLambda = codeGenerator.Translate srcLambda translatorOptions
        let code = Printer.AST.Print ast
        printfn "Code = %A" code
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
                        let c =
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
                        <@@
                            let x = %%c |> List.ofArray
                            range := (box x.Head) :?> 'TRange
                            args := x.Tail |> Array.ofList
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
    | MsgNotifyMe of AsyncReplyChannel<unit>
    | MsgBarrier of SyncObject

    static member CreateToHostMsg m =
        {
            new ToHostCrate with
                member this.Apply e = e.Eval m
        }
        |> MsgToHost

    static member CreateToGPUMsg m =
        {
            new ToGPUCrate with
                member this.Apply e = e.Eval m
        }
        |> MsgToGPU

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
        printfn "Allocation"
        let buf = new Brahma.OpenCL.Buffer<_>(clContext, Brahma.OpenCL.Operations.ReadWrite, true, length)
        let res = new GpuArray<'t>(buf,length)
        res

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
                                then raise (Cl.Exception error)

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

        printfn "MB is started"
        let rec loop i = async {
            let! msg = inbox.Receive()
            match msg with
            | MsgToHost a ->
                printfn "ToHost"
                this.HandleToHost(commandQueue, a) |> ignore

            | MsgToGPU a ->
                printfn "ToGPU"
                this.HandleToGPU(commandQueue, a) |> ignore

            | MsgRun a ->
                printfn "Run"
                this.HandleRun(commandQueue, a) |> ignore

            | MsgNotifyMe ch ->
                printfn "NotifyMe"
                OpenCL.Net.Cl.Finish(commandQueue)
                ch.Reply ()

            | MsgBarrier o ->
                printfn "Barrier"
                OpenCL.Net.Cl.Finish(commandQueue)
                o.ImReady()
                while not <| o.CanContinue() do ()

            return! loop 0
            }
        loop 0)

module Lib =

    let getNewMAddM<'t> (gpu: GPU) op =
        let kernelFun =
            <@
                fun (r:_2D) mSize (a:array<'t>) (b:array<'t>) (c:array<'t>) ->
                    let tx = r.GlobalID0
                    let ty = r.GlobalID1
                    c.[ty * mSize + tx] <- (%op) a.[ty * mSize + tx] b.[ty * mSize + tx]
            @>

        let kernel = gpu.CreateKernel(kernelFun)
        fun (processor:MailboxProcessor<_>) rng mSize (a:GpuArray<'t>) (b:GpuArray<'t>) (res:GpuArray<'t>) ->
            kernel.SetArguments rng mSize a b res
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

    let getNewMxMAdd<'t1,'t2,'t3> (gpu: GPU) opAdd opMult =
        let kernelFun =
            <@
                fun (r:_2D) mSize (a:array<'t1>) (b:array<'t2>) (c:array<'t3>) ->
                    let tx = r.GlobalID0
                    let ty = r.GlobalID1
                    let mutable buf = c.[ty * mSize + tx]
                    for k in 0 .. mSize - 1 do
                        buf <- (%opAdd) buf  ((%opMult) a.[ty * mSize + k]  b.[k * mSize + tx])
                    c.[ty * mSize + tx] <- buf
            @>

        let kernel = gpu.CreateKernel(kernelFun)

        fun (processor:MailboxProcessor<_>) rng mSize (a:GpuArray<'t1>) (b:GpuArray<'t2>) (res:GpuArray<'t3>) ->
            kernel.SetArguments rng mSize a b res
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

    let getNewVectorVectorElementwiseOp<'t1,'t2,'t3> (gpu:GPU) op =
        let kernelFun =
            <@ fun (range:_1D) (a1:array<'t1>) (a2:array<'t2>) (res:array<'t3>) ->
                let i = range.GlobalID0
                res.[i] <- (%op) a1.[i] a2.[i] @>
        let kernel = gpu.CreateKernel(kernelFun)
        fun (processor:MailboxProcessor<_>) rng (a1:GpuArray<'t1>) (a2:GpuArray<'t2>) (res:GpuArray<'t3>) ->
            kernel.SetArguments rng a1 a2 res
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

type Host() =

    let kernelFun =
        <@ fun (range:_1D) (buf:array<_>) n ->
            let i = range.GlobalID0
            buf.[i] <- buf.[i] * n @>

    let mxmQuadro () =

        let devices = Device.getDevices "*NVIDIA*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        let processors = Array.init 4 (fun _ -> gpu.GetNewProcessor ())

        let mSize = 3 * 1024
        let size = mSize * mSize
        let localWorkSize = 32
        let d = (new _2D(mSize, mSize, localWorkSize, localWorkSize))

        let aBlocks = Array.init 4 (fun _ -> Array.init size (fun _ -> 1))
        let bBlocks = Array.init 4 (fun _ -> Array.init size (fun _ -> 2))
        let resBlocks = Array.init 4 (fun _ -> Array.zeroCreate size)

        let _aBlocks = Array.init 4 (fun _ -> gpu.Allocate<_>(size))
        let _bBlocks = Array.init 4 (fun _ -> gpu.Allocate<_>(size))
        let _resBlocks = Array.init 4 (fun _ ->  gpu.Allocate<_>(size))

        let mxm = Array.init 4 (fun _ -> Lib.getNewMxMAdd<_,_,_> gpu <@ (+) @> <@ (*) @>)
        let mam = Array.init 4 (fun _ -> Lib.getNewMAddM<   int> gpu <@ (+) @>)

        let barrier1 = Msg.CreateBarrierMessages 4

        processors
        |> Array.iteri (fun i p ->
            p.Post(Msg.CreateToGPUMsg(ToGPU<_>(aBlocks.[i], _aBlocks.[i])))
            p.Post(Msg.CreateToGPUMsg(ToGPU<_>(bBlocks.[i], _bBlocks.[i])))
            p.Post(Msg.CreateToGPUMsg(ToGPU<_>(resBlocks.[i], _resBlocks.[i])))
            p.Post(barrier1.[i])
            )

        for i in 0..1 do
            for j in 0..1 do
                for k in 0..1 do
                    let mxm = mxm.[i * 2 + j]
                    let mam = mam.[i * 2 + j]
                    mxm processors.[i * 2 + j] d mSize _aBlocks.[i * 2 + k] _bBlocks.[k * 2 + j] _resBlocks.[i * 2 + j]

        let barrier2 = Msg.CreateBarrierMessages 4
        Array.iteri2
            (fun i (p:MailboxProcessor<_>) b ->
                p.Post(b)
            )
            processors
            barrier2

        processors
        |> Array.mapi (fun i p -> p.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(_resBlocks.[i], resBlocks.[i], ch))))

        resBlocks
        |> Array.iter (fun x -> printfn "%A" x)



    let f1 () =
        let devices = Device.getDevices "*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        let processor1 = gpu.GetNewProcessor ()
        let processor2 = gpu.GetNewProcessor ()
        let a1 = Array.init 10 (fun i -> i + 1)
        let a2 = Array.init 20 (fun i -> float i + 2.0)
        let res1 = Array.zeroCreate 10
        let res2 = Array.zeroCreate 20

        let n1 = gpu.Allocate<_>(a1.Length)
        let n2 = gpu.Allocate<_>(a1.Length)
        let nRes = gpu.Allocate<_>(a1.Length)

        let m1 = gpu.Allocate<_>(a2.Length)
        let m2 = gpu.Allocate<_>(a2.Length)
        let mRes = gpu.Allocate<_>(a2.Length)

        processor1.Post(Msg.CreateToGPUMsg(ToGPU<_>(a1, n1)))
        processor1.Post(Msg.CreateToGPUMsg(ToGPU<_>(a1, n2)))

        processor2.Post(Msg.CreateToGPUMsg(ToGPU<_>(a2, m1)))
        processor2.Post(Msg.CreateToGPUMsg(ToGPU<_>(a2, m2)))

        let vAdd = Lib.getNewVectorVectorElementwiseOp<_,_,_> gpu <@ (+) @>
        let vMult = Lib.getNewVectorVectorElementwiseOp<_,_,_> gpu <@ (*) @>

        let _1d1 = new _1D(a1.Length,1)
        let _1d2 = new _1D(a2.Length,1)

        let r1 = vAdd processor2 _1d1 n1 n2 nRes
        let r2 = vMult processor1 _1d2 m1 m2 mRes
        //kernel.SetArguments _1d m1 6

        //processor1.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

        //This code is unsafe because there is no synchronization between processors
        //It is just to show that we can share buffers between queues on the same device
        let res1 = processor2.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(nRes, res1, ch)))
        let res2 = processor1.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(mRes, res2, ch)))

        let result =
            res1,res2

        result


    member this.Do () =
           mxmQuadro ()
           [||],[||]
