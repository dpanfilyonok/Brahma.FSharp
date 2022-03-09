namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL
open Brahma.FSharp.OpenCL.Translator
open System.Runtime.InteropServices

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
        DefaultCompilerOptions: string option

        CommandQueue: MailboxProcessor<Msg>
        RuntimeOptions: RuntimeOptions
    }

    static member Create
        (
            clContext: ClContext,
            translator: FSQuotationToOpenCLTranslator,
            ?defaultCompilerOptions: string,
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
        | None -> CompilationContext.Create(this.ClContext, this.Translator, this.DefaultCompilerOptions)
        | Some compilerOptions -> CompilationContext.Create(this.ClContext, this.Translator, Some compilerOptions)
