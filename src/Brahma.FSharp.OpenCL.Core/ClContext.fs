namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
open FSharp.Quotations
open System.Runtime.InteropServices


exception EmptyDevicesException of string

[<RequireQualifiedAccess>]
type ClPlatform =
    | Intel
    | Amd
    | Nvidia
    | Any
    | Custom of pattern: string

    static member internal ConvertToPattern(platform: ClPlatform) =
        match platform with
        | Intel -> "Intel*"
        | Amd -> "AMD*"
        | Nvidia -> "NVIDIA*"
        | Any -> "*"
        | Custom pattern -> pattern

[<RequireQualifiedAccess>]
type ClDeviceType =
    | CPU
    | GPU
    | Default

    static member internal ConvertToDeviceType(deviceType: ClDeviceType) =
        match deviceType with
        | CPU -> DeviceType.Cpu
        | GPU -> DeviceType.Gpu
        | Default -> DeviceType.Default

// TODO redesign
module internal Device =
    open System.Text.RegularExpressions

    let private wildcardToRegex (pattern: string) =
        "^" + Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".") + "$"

    let getDevices platformName deviceType =
        let platformNameRegex = Regex(wildcardToRegex platformName, RegexOptions.IgnoreCase)
        let error = ref Unchecked.defaultof<ErrorCode>

        Cl.GetPlatformIDs error
        |> Array.choose
            (fun platform ->
                let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, error).ToString()
                if platformNameRegex.Match(platformName).Success then
                    Some <| Cl.GetDeviceIDs(platform, deviceType, error)
                else
                    None
            )
        |> Array.concat

    let getFirstAppropriateDevice platformName deviceType =
        try
            (getDevices platformName deviceType).[0]
        with
        | :? System.ArgumentException as ex ->
            raise <| EmptyDevicesException $"No %A{deviceType} devices on platform %A{platformName} were found"

type ClContext =
    {
        Device: Device
        Context: Context
    }

    interface IContext with
        member this.Device = this.Device
        member this.Context = this.Context

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

type CompilationContext
    (
        clContext: IContext,
        translator: FSQuotationToOpenCLTranslator,
        [<Optional>] compilerOptions: string
    ) =

    member this.Compile(srcLambda: Expr<'a -> 'b>) =
        ClProgram(clContext, translator, srcLambda, compilerOptions)

type RuntimeContext2 =
    private {
        ClContext: ClContext
        Translator: FSQuotationToOpenCLTranslator
        CommandQueue: MailboxProcessor<Msg>
        RuntimeOptions: RuntimeOptions
    }

    static member Create
        (
            device: Device,
            [<Optional>] translatorOptions: TranslatorOptions,
            [<Optional>] compilerOptions: string,
            ?runtimeOptions: RuntimeOptions
        ) =

        let runtimeOptions = defaultArg runtimeOptions RuntimeOptions.Default

        let context =
            let error = ref Unchecked.defaultof<ErrorCode>
            let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

            if error.Value <> ErrorCode.Success then
                raise <| Cl.Exception error.Value

            ctx

        let clContext =
            {
                Device = device
                Context = context
            }

        let translator = FSQuotationToOpenCLTranslator(translatorOptions (*, deviceInfo*))
        let queue = CommandQueueProvider.CreateQueue(context, device)

        {
            ClContext = clContext
            Translator = translator
            CommandQueue = queue
            RuntimeOptions = runtimeOptions
        }

type RuntimeContext
    (
        device: Device,
        [<Optional>] translatorOptions: TranslatorOptions,
        [<Optional>] compilerOptions: string,
        ?runtimeOptions: RuntimeOptions
    ) =

    let runtimeOptions = defaultArg runtimeOptions RuntimeOptions.Default

    let context =
        let error = ref Unchecked.defaultof<ErrorCode>
        let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

        if error.Value <> ErrorCode.Success then
            raise <| Cl.Exception error.Value

        ctx

    let clContext =
        {
            Device = device
            Context = context
        }

    let translator = FSQuotationToOpenCLTranslator(translatorOptions (*, deviceInfo*))
    let queue = CommandQueueProvider.CreateQueue(context, device)

    new([<Optional>] translatorOptions: TranslatorOptions,
        [<Optional>] compilerOptions: string,
        [<Optional>] runtimeOptions: RuntimeOptions,
        ?platform: ClPlatform,
        ?deviceType: ClDeviceType) =

        let platform = defaultArg platform ClPlatform.Any
        let deviceType = defaultArg deviceType ClDeviceType.Default

        let device =
            Device.getFirstAppropriateDevice
            <| ClPlatform.ConvertToPattern platform
            <| ClDeviceType.ConvertToDeviceType deviceType

        RuntimeContext(device, translatorOptions, compilerOptions, runtimeOptions)

    member this.RuntimeOptions = runtimeOptions

    member this.GetCompilationContext() = CompilationContext(clContext, translator, compilerOptions)

    member this.WithNewCommandQueue() =
        { new IRuntimeContext with
            member _.ClContext = clContext :> IContext
            member _.Translator = translator
            member _.CommandQueue = CommandQueueProvider.CreateQueue(context, device)
        }

    interface IRuntimeContext with
        member _.ClContext = clContext :> IContext
        member _.Translator = translator
        member _.CommandQueue = queue

    // TODO rewrite
    member _.ClContext = clContext
    member _.Translator = translator
    member _.CommandQueue = queue
