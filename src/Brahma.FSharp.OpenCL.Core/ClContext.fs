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

module Device =
    open System.Text.RegularExpressions

    let private wildcardToRegex (pattern: string) =
        "^" + Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".") + "$"

    let internal getDevices platformName deviceType =
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

    let getAllDevices platform deviceType =
        getDevices (ClPlatform.ConvertToPattern platform) (ClDeviceType.ConvertToDeviceType deviceType)

    let internal getFirstAppropriateDevice platformName deviceType =
        try
            (getDevices platformName deviceType).[0]
        with
        | :? System.ArgumentException as ex ->
            raise <| EmptyDevicesException(sprintf "No %A devices on platform %A were found" deviceType platformName)

type ClContext private (context: Context, device: Device, translator: FSQuotationToOpenCLTranslator, provider: ComputeProvider) =
    new (device: Device) =
        let context =
            let error = ref Unchecked.defaultof<ErrorCode>
            let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

            if !error <> ErrorCode.Success then
                raise <| Cl.Exception !error

            ctx

        let translator = FSQuotationToOpenCLTranslator()
        let provider = ComputeProvider(context, device)
        
        ClContext(context, device, translator, provider)

    new (?platform: ClPlatform, ?deviceType: ClDeviceType) =
        let platform = defaultArg platform ClPlatform.Any
        let deviceType = defaultArg deviceType ClDeviceType.Default

        let device =
            Device.getFirstAppropriateDevice
            <| ClPlatform.ConvertToPattern platform
            <| ClDeviceType.ConvertToDeviceType deviceType

        
        ClContext(device)

    interface IContext with
        member this.Context = context
        member this.Device = device
        member this.Translator = translator
        member this.Provider = provider

    override this.ToString() =
        let mutable e = ErrorCode.Unknown
        let context = this
        let device = context.Device
        let deviceName = Cl.GetDeviceInfo(device, DeviceInfo.Name, &e).ToString()
        if deviceName.Length < 20 then
            sprintf "%s" deviceName
        else
            let platform = Cl.GetDeviceInfo(device, DeviceInfo.Platform, &e).CastTo<Platform>()
            let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, &e).ToString()
            let deviceType =
                match Cl.GetDeviceInfo(device, DeviceInfo.Type, &e).CastTo<DeviceType>() with
                | DeviceType.Cpu -> "CPU"
                | DeviceType.Gpu -> "GPU"
                | DeviceType.Accelerator -> "Accelerator"
                | _ -> "another"

            sprintf "%s, %s" platformName deviceType

    member this.Context = (this :> IContext).Context
    member this.Device = (this :> IContext).Device
    member this.Translator = (this :> IContext).Translator
    member this.Provider = (this :> IContext).Provider

    member this.WithNewComputeProvider() =
        ClContext(this.Context, this.Device, this.Translator, ComputeProvider(this.Context, this.Device))

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
