namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations
open System.Runtime.InteropServices

type CompilationContext
    (
        clContext: ClContext,
        translator: FSQuotationToOpenCLTranslator,
        [<Optional>] compilerOptions: string
    ) =

    member this.Compile(srcLambda: Expr<'a -> 'b>) =
        ClProgram(clContext, translator, srcLambda, compilerOptions)

type RuntimeOptions =
    {
        // TODO if 2D or 3D
        WorkgroupSize: int
        HostAccessMode: HostAccessMode
        DeviceAccessMode: DeviceAccessMode
        AllocationModeIfData: AllocationMode
        AllocationModeIfNoData: AllocationMode
    }

    static member Default =
        {
            WorkgroupSize = 256
            HostAccessMode = HostAccessMode.ReadWrite
            DeviceAccessMode = DeviceAccessMode.ReadWrite
            AllocationModeIfData = AllocationMode.AllocAndCopyHostPtr
            AllocationModeIfNoData = AllocationMode.AllocHostPtr
        }

type RuntimeContext =
    private {
        ClContext: ClContext
        Translator: FSQuotationToOpenCLTranslator
        QueueProvider: CommandQueueProvider
        DefaultCompilerOptions: string

        CommandQueue: MailboxProcessor<Msg>
        RuntimeOptions: RuntimeOptions
    }

    static member Create
        (
            clContext: ClContext,
            translator: FSQuotationToOpenCLTranslator,
            [<Optional>] defaultCompilerOptions: string,
            ?runtimeOptions: RuntimeOptions
        ) =

        let runtimeOptions = defaultArg runtimeOptions RuntimeOptions.Default

        let queueProvider = CommandQueueProvider(clContext, translator)

        let queue = queueProvider.CreateQueue()

        {
            ClContext = clContext
            Translator = translator
            QueueProvider = queueProvider
            DefaultCompilerOptions = defaultCompilerOptions

            CommandQueue = queue
            RuntimeOptions = runtimeOptions
        }

    member internal this.WithNewCommandQueue() =
        { this with CommandQueue = this.QueueProvider.CreateQueue() }

    member internal this.WithRuntimeOptions(runtimeOptions) =
        { this with RuntimeOptions = runtimeOptions }

    member this.GetCompilationContext(?compilerOptions: string) =
        match compilerOptions with
        | None -> CompilationContext(this.ClContext, this.Translator, this.DefaultCompilerOptions)
        | Some compilerOptions -> CompilationContext(this.ClContext, this.Translator, compilerOptions)


//type RuntimeContext
//    (
//        clContext: ClContext,
//        translator: FSQuotationToOpenCLTranslator,
//        [<Optional>] defaultCompilerOptions: string,
//        ?runtimeOptions: RuntimeOptions
//    ) =
//
//    let runtimeOptions = defaultArg runtimeOptions RuntimeOptions.Default
//
//    let mutable queueProvider = CommandQueueProvider(clContext, translator)
//
//    let mutable queue = queueProvider.CreateQueue()
//
//    // TODO а зачем get
//    member this.QueueProvider
//        with get() = queueProvider
//        and private set(value) = queueProvider <- value
//
//    // TODO а зачем get
//    member this.CommandQueue
//        with get() = queue
//        and private set(value) = queue <- value
//
//    member this.ClContext = clContext
//
//    member this.Translator = translator
//
//    member this.RuntimeOptions = runtimeOptions
//
//    member this.GetCompilationContext(?compilerOptions: string) =
//        match compilerOptions with
//        | None -> CompilationContext(clContext, translator, defaultCompilerOptions)
//        | Some compilerOptions -> CompilationContext(clContext, translator, compilerOptions)
//
//    member internal this.WithNewCommandQueue() =
//        RuntimeContext(clContext, translator, defaultCompilerOptions, runtimeOptions, QueueProvider = queueProvider)
//
//    member internal this.WithRuntimeOptions(runtimeOptions) =
//        RuntimeContext(clContext, translator, defaultCompilerOptions, runtimeOptions, CommandQueue = queue)
