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

type GpuKernel<'TRange, 'a when 'TRange :> Brahma.OpenCL.INDRangeDimension>(device, context, srcLambda: Expr<'TRange ->'a>) =

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
        //var isIMem = arg is IMem;
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
    let argsSetupFunction =
        let rng = ref Unchecked.defaultof<'TRange>
        let args = ref [||]
        //let run = ref Unchecked.defaultof<Commands.Run<'TRange>>
        let getStarterFunction qExpr =
            let rec go expr vars =
                match expr with
                | Patterns.Lambda (v, body) ->
                    Expr.Lambda(v, go body (v::vars))
                | e ->
                    let arr =
                        let c =
                            Expr.NewArray(
                                    typeof<obj>,
                                    vars |> List.rev |> List.map (fun v -> Expr.Coerce (Expr.Var(v), typeof<obj>))
                                    )
                        <@@
                            let x = %%c |> List.ofArray
                            range := (box x.Head) :?> 'TRange
                            args := x.Tail |> Array.ofList
                            //let brahmsRunCls = new Brahma.OpenCL.Commands.Run<_>(kernel :> Kernel<'TRange>,!rng)
                            !args |> Array.iteri (fun i x -> setupArgument i x)
                            //run := kernel.Run(!rng, !args)
                        @@>
                    arr
            let res = <@ %%(go qExpr []):'TRange ->'a @>.Compile()

            res
        getStarterFunction srcLambda
    member this.SetArguments = argsSetupFunction
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

type Run<'TRange,'t when 'TRange :> Brahma.OpenCL.INDRangeDimension>(kernel:GpuKernel<'TRange,'t>, ?replyChannel:AsyncReplyChannel<bool>) =
    member this.Kernel = kernel
    member this.ReplyChannel = replyChannel

type RunCrate =
    abstract member Apply<'ret> : RunCrateEvaluator<'ret> -> 'ret

and RunCrateEvaluator<'ret> =
    abstract member Eval<'TRange,'t when 'TRange :> Brahma.OpenCL.INDRangeDimension> : Run<'TRange, 't> -> 'ret

type ToHostCrate =
    abstract member Apply<'ret> : ToHostCrateEvaluator<'ret> -> 'ret

and ToHostCrateEvaluator<'ret> =
    abstract member Eval<'a> : ToHost<'a> -> 'ret

type ToGPUCrate =
    abstract member Apply<'ret> : ToGPUCrateEvaluator<'ret> -> 'ret

and ToGPUCrateEvaluator<'ret> =
    abstract member Eval<'a> : ToGPU<'a> -> 'ret

type Msg =
    | MsgToHost of ToHostCrate
    | MsgToGPU of ToGPUCrate
    | MsgRun of RunCrate

    static member CreateToHostMsg m =
        {
            new ToHostCrate with
                member __.Apply e = e.Eval m
        }
        |> MsgToHost

    static member CreateToGPUMsg m =
        {
            new ToGPUCrate with
                member __.Apply e = e.Eval m
        }
        |> MsgToGPU

    static member CreateRunMsg m =
        {
            new RunCrate with
                member __.Apply e = e.Eval m
        }
        |> MsgRun
type GPU(device: Device) =

    let clContext =
        let error = ref (Unchecked.defaultof<ErrorCode>)
        let ctx = Cl.CreateContext(null, 1u, [|device|], null, System.IntPtr.Zero, error)
        if (!error <> ErrorCode.Success)
        then raise (new Cl.Exception(!error))
        ctx

    member this.ClDevice = device

    member this.ClContext = clContext

    member this.CreateKernel (srcLambda) =
        new GpuKernel<_,_>(device, clContext, srcLambda)

    member this.Allocate<'t> (length:int) =
        printfn "Allocation"
        let buf = new Brahma.OpenCL.Buffer<_>(clContext, Brahma.OpenCL.Operations.ReadWrite, true, length)
        let res = new GpuArray<'t>(buf,length)
        res

    member private this.HandleToGPU (queue:Brahma.OpenCL.CommandQueue, toGpu:ToGPUCrate) =
        toGpu.Apply
                {
                    new ToGPUCrateEvaluator<int>
                    with member __.Eval (a) =
                            let write (src:array<'t>) (dst:GpuArray<'t>) =
                                let eventID = ref Unchecked.defaultof<Event>

                                let mem = dst.Buffer.Mem
                                let elementSize = dst.Buffer.ElementSize
                                let error = Cl.EnqueueWriteBuffer(queue.Queue, mem,
                                            Bool.False, System.IntPtr(0),
                                            System.IntPtr(dst.Length * elementSize), src, 0u, null, eventID);

                                if error <> ErrorCode.Success
                                then
                                    printfn "Error in write: %A" error
                                    raise (Cl.Exception(error))

                            write a.Source a.Destination
                            0
                }
    member private this.HandleToHost (queue:Brahma.OpenCL.CommandQueue, toHost:ToHostCrate) =
        toHost.Apply
                {
                    new ToHostCrateEvaluator<int>
                    with member __.Eval (a) =
                            let read (src:GpuArray<'t>) (dst:array<'t>)=
                                let eventID = ref Unchecked.defaultof<Event>
                                let mem = src.Buffer.Mem
                                let elementSize = src.Buffer.ElementSize
                                let error = Cl.EnqueueReadBuffer(queue.Queue, mem,
                                            Bool.False, System.IntPtr(0),
                                            System.IntPtr(src.Length * elementSize), dst, 0u, null, eventID);

                                if error <> ErrorCode.Success
                                then raise (Cl.Exception(error))
                                dst
                            let res = read a.Source a.Destination
                            match a.ReplyChannel with
                            | None -> ()
                            | Some ch ->
                                OpenCL.Net.Cl.Finish(queue.Queue)
                                ch.Reply res
                            0
                }

    member private this.HandleRun (queue:Brahma.OpenCL.CommandQueue, run:RunCrate) =
        run.Apply
                {
                    new RunCrateEvaluator<int>
                    with member __.Eval (a) =
                            let runKernel (kernel:GpuKernel<'TRange,'t>) =
                                let range = kernel.Range
                                //(INDRangeDimension)
                                let workDim = uint32 range.Dimensions
                                let eventID = ref Unchecked.defaultof<Event>
                                let error =
                                    Cl.EnqueueNDRangeKernel(queue.Queue, kernel.ClKernel, workDim, null,
                                                            range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID);
                                if error <> ErrorCode.Success
                                then raise (Cl.Exception(error))

                            runKernel a.Kernel

                            match a.ReplyChannel with
                            | None -> ()
                            | Some ch ->
                                OpenCL.Net.Cl.Finish(queue.Queue)
                                ch.Reply true
                            0
                }

    member this.GetNewProcessor () = MailboxProcessor.Start(fun inbox ->

        let commandQueue = new Brahma.OpenCL.CommandQueue(clContext, device)

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

            return! loop 0
            }
        loop 0)

type Host() =

    let kernelFun = <@fun (range:_1D) (buf:array<_>) -> buf.[0] <- 1L@>

    member this.Do () =
        let devices = Device.getDevices "*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        let processor1 = gpu.GetNewProcessor ()
        let processor2 = gpu.GetNewProcessor ()
        let a1 = Array.init 10 (fun i -> i + 1)
        let a2 = Array.init 20 (fun i -> float i + 2.0)
        let res1 = Array.zeroCreate 10
        let res2 = Array.zeroCreate 20

        let m1 = gpu.Allocate<_>(a1.Length)
        let m2 = gpu.Allocate<_>(a2.Length)

        processor1.Post(Msg.CreateToGPUMsg(ToGPU<_>(a1, m1)))
        processor2.Post(Msg.CreateToGPUMsg(ToGPU<_>(a2, m2)))

        let kernel = gpu.CreateKernel(kernelFun)

        printfn "Kernel: %A" kernel
        printfn "Kernel.SetArguments: %A" kernel.SetArguments

        //This code is unsafe because there is no synchronization between processors
        //It is just to show that we can share buffers between queues on the same device
        let res1 = processor2.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(m1, res1, ch)))
        let res2 = processor1.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(m2, res2, ch)))

        let result =
            res1,res2

        result
