namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

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
            raise <| EmptyDevicesException(sprintf "No %A devices on platform %A were found" deviceType platformName)

type ClContext private (context: Context, device: Device, translator: FSQuotationToOpenCLTranslator, queue: MailboxProcessor<Msg>) =
    new (?platform: ClPlatform, ?deviceType: ClDeviceType) =
        let platform = defaultArg platform ClPlatform.Any
        let deviceType = defaultArg deviceType ClDeviceType.Default

        let device =
            Device.getFirstAppropriateDevice
            <| ClPlatform.ConvertToPattern platform
            <| ClDeviceType.ConvertToDeviceType deviceType

        let context =
            let error = ref Unchecked.defaultof<ErrorCode>
            let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

            if !error <> ErrorCode.Success then
                raise <| Cl.Exception !error

            ctx

        let translator = FSQuotationToOpenCLTranslator()
        let queue = CommandQueueProvider.CreateQueue(context, device)

        ClContext(context, device, translator, queue)

    interface IContext with
        member this.Context = context
        member this.Device = device
        member this.Translator = translator
        member this.CommandQueue = queue

    member this.Context = (this :> IContext).Context
    member this.Device = (this :> IContext).Device
    member this.Translator = (this :> IContext).Translator
    member this.CommandQueue = (this :> IContext).CommandQueue

    member this.WithNewCommandQueue() =
        ClContext(this.Context, this.Device, this.Translator, CommandQueueProvider.CreateQueue(this.Context, this.Device))

    member this.CreateClKernel(srcLambda: Expr<'a -> 'b>) =
        ClKernel<_,_>(this, srcLambda)

    member this.CreateClBuffer
        (
            data: 'a[],
            ?hostAccessMode: HostAccessMode,
            ?deviceAccessMode: DeviceAccessMode,
            ?allocationMode: AllocationMode
        ) =

        let hostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
        let deviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
        let allocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode

        new ClBuffer<'a>(
            this,
            Data data,
            {
                HostAccessMode = hostAccessMode
                DeviceAccessMode = deviceAccessMode
                AllocationMode = allocationMode
            }
        )

    member this.CreateClBuffer
        (
            size: int,
            ?hostAccessMode: HostAccessMode,
            ?deviceAccessMode: DeviceAccessMode,
            ?allocationMode: AllocationMode
        ) =

        let hostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
        let deviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
        let allocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode

        new ClBuffer<'a>(
            this,
            Size size,
            {
                HostAccessMode = hostAccessMode
                DeviceAccessMode = deviceAccessMode
                AllocationMode = allocationMode
            }
        )
