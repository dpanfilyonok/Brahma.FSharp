namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Brahma.FSharp.OpenCL.Shared

type ClPlatform = OpenCL.Net.Platform
type ClDeviceType = OpenCL.Net.DeviceType

exception EmptyDevicesException of string

module internal DeviceHelpers =
    let convertToDeviceType(deviceType: DeviceType) =
        match deviceType with
        | DeviceType.CPU -> ClDeviceType.Cpu
        | DeviceType.GPU -> ClDeviceType.Gpu
        | DeviceType.Default -> ClDeviceType.Default

type ClDevice(device: Device) =
    let throwOnError f =
        let error = ref Unchecked.defaultof<ErrorCode>
        let result = f error
        if error.Value <> ErrorCode.Success then
            failwithf $"Program creation failed: %A{error}"
        else
            result

    let defaultOnError onError f =
        let error = ref Unchecked.defaultof<ErrorCode>
        let result = f error
        if error.Value <> ErrorCode.Success then
            onError
        else
            result

    let (|Contains|_|) (substring: string) (str: string) =
        if str.Contains substring then Some Contains
        else None

    member this.Device = device

    interface IDevice with
        member val Name =
            fun e -> Cl.GetDeviceInfo(device, DeviceInfo.Name, e).ToString()
            |> defaultOnError ""

        member val Platform =
            fun e ->
                match Cl.GetDeviceInfo(device, DeviceInfo.Vendor, e).ToString() with
                | Contains "NVIDIA" -> Platform.Nvidia
                | Contains "Intel" -> Platform.Intel
                | Contains "AMD" -> Platform.Amd
                | _ -> Platform.Any
            |> defaultOnError Platform.Any

        member val DeviceType =
            fun e ->
                match Cl.GetDeviceInfo(device, DeviceInfo.Type, e).CastTo<ClDeviceType>() with
                | ClDeviceType.Cpu -> DeviceType.CPU
                | ClDeviceType.Gpu -> DeviceType.GPU
                | _ -> DeviceType.Default
            |> defaultOnError DeviceType.Default

        member val MaxWorkGroupSize =
            fun e -> Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkGroupSize, e).CastTo<int>()
            |> throwOnError

        member val MaxWorkItemDimensions =
            fun e -> Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkItemDimensions, e).CastTo<int>()
            |> throwOnError

        // TODO change length
        member val MaxWorkItemSizes =
            fun e -> Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkItemSizes, e).CastToArray<int>(3)
            |> throwOnError

        member val DeviceExtensions =
            fun e -> Cl.GetDeviceInfo(device, DeviceInfo.Extensions, e).ToString()
            |> throwOnError

    static member GetAllDevices(?platform: Platform, ?deviceType: DeviceType) =
        let platform = defaultArg platform Platform.Any
        let deviceType = defaultArg deviceType DeviceType.Default

        let error = ref Unchecked.defaultof<ErrorCode>

        Cl.GetPlatformIDs error
        |> Array.choose
            (fun platform ->
                let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, error).ToString()
                if platformName.ToLower().Contains($"{platform}".ToLower()) then
                    Some <| Cl.GetDeviceIDs(platform, DeviceHelpers.convertToDeviceType deviceType, error)
                else
                    None
            )
        |> Array.concat

    static member GetFirstAppropriateDevice(?platform: Platform, ?deviceType: DeviceType) =
        let platform = defaultArg platform Platform.Any
        let deviceType = defaultArg deviceType DeviceType.Default

        try
            ClDevice(ClDevice.GetAllDevices(platform, deviceType).[0])
        with
        | :? System.ArgumentException as ex ->
            raise <| EmptyDevicesException $"No %A{deviceType} devices on platform %A{platform} were found"
