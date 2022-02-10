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

type RuntimeContext
    (
        clContext: ClContext,
        translator: FSQuotationToOpenCLTranslator,
        [<Optional>] defaultCompilerOptions: string,
        ?runtimeOptions: RuntimeOptions
    ) =

    let runtimeOptions = defaultArg runtimeOptions RuntimeOptions.Default

    let mutable queue = CommandQueueProvider.CreateQueue(clContext)

    member this.CommandQueue
        with get() = queue
        and private set(value) = queue <- value

    member this.ClContext = clContext

    member this.Translator = translator

    member this.RuntimeOptions = runtimeOptions

    member this.GetCompilationContext(?compilerOptions: string) =
        match compilerOptions with
        | None -> CompilationContext(clContext, translator, defaultCompilerOptions)
        | Some compilerOptions -> CompilationContext(clContext, translator, compilerOptions)

    member this.WithNewCommandQueue() =
        RuntimeContext(clContext, translator, defaultCompilerOptions, runtimeOptions)

    member this.WithRuntimeOptions(runtimeOptions) =
        RuntimeContext(clContext, translator, defaultCompilerOptions, runtimeOptions, CommandQueue = this.CommandQueue)
