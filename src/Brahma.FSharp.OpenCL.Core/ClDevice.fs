﻿namespace Brahma.FSharp

open System.Text.RegularExpressions

type ClPlatform = OpenCL.Net.Platform
type ClDeviceType = OpenCL.Net.DeviceType
type ClErrorCode = OpenCL.Net.ErrorCode
type Cl = OpenCL.Net.Cl

exception EmptyDevicesException of string

module internal DeviceHelpers =
    let convertToDeviceType(deviceType: DeviceType) =
        match deviceType with
        | DeviceType.CPU -> ClDeviceType.Cpu
        | DeviceType.GPU -> ClDeviceType.Gpu
        | DeviceType.Default -> ClDeviceType.Default

    let convertToPattern(platform: Platform) =
        match platform with
        | Platform.Intel -> "Intel*"
        | Platform.Amd -> "AMD*"
        | Platform.Nvidia -> "NVIDIA*"
        | Platform.Any -> "*"
        | Platform.Custom pattern -> pattern

type ClDevice(device: OpenCL.Net.Device) =
    let throwOnError f =
        let error = ref Unchecked.defaultof<ClErrorCode>
        let result = f error
        if error.Value <> ClErrorCode.Success then
            failwithf $"Program creation failed: %A{error}"
        else
            result

    let defaultOnError onError f =
        let error = ref Unchecked.defaultof<ClErrorCode>
        let result = f error
        if error.Value <> ClErrorCode.Success then
            onError
        else
            result

    let (|Contains|_|) (substring: string) (str: string) =
        if str.Contains substring then Some Contains
        else None

    member this.Device = device

    interface IDevice with
        member val Name =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Name, e).ToString()
            |> defaultOnError ""

        member val Platform =
            fun e ->
                match Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Vendor, e).ToString() with
                | Contains "NVIDIA" -> Platform.Nvidia
                | Contains "Intel" -> Platform.Intel
                | Contains "AMD" -> Platform.Amd
                | _ -> Platform.Any
            |> defaultOnError Platform.Any

        member val DeviceType =
            fun e ->
                match Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Type, e).CastTo<ClDeviceType>() with
                | ClDeviceType.Cpu -> DeviceType.CPU
                | ClDeviceType.Gpu -> DeviceType.GPU
                | _ -> DeviceType.Default
            |> defaultOnError DeviceType.Default

        member val MaxWorkGroupSize =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkGroupSize, e).CastTo<int>()
            |> throwOnError

        member val MaxWorkItemDimensions =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkItemDimensions, e).CastTo<int>()
            |> throwOnError

        // TODO change length
        member val MaxWorkItemSizes =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkItemSizes, e).CastToArray<int>(3)
            |> throwOnError

        member val DeviceExtensions =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Extensions, e).ToString()
            |> throwOnError

    member this.Name = (this :> IDevice).Name
    member this.Platform = (this :> IDevice).Platform
    member this.DeviceType = (this :> IDevice).DeviceType
    member this.MaxWorkGroupSize = (this :> IDevice).MaxWorkGroupSize
    member this.MaxWorkItemDimensions = (this :> IDevice).MaxWorkItemDimensions
    member this.MaxWorkItemSizes = (this :> IDevice).MaxWorkItemSizes
    member this.DeviceExtensions = (this :> IDevice).DeviceExtensions

    override this.ToString() =
        $"{(this :> IDevice).Name} | {(this :> IDevice).Platform} | {(this :> IDevice).DeviceType}"

    static member GetAvailableDevices(?platform: Platform, ?deviceType: DeviceType) =
        let platform = defaultArg platform Platform.Any
        let deviceType = defaultArg deviceType DeviceType.Default

        let wildcardToRegex (pattern: string) =
            "^" + Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".") + "$"

        let platformNameRegex = Regex(wildcardToRegex <| DeviceHelpers.convertToPattern platform, RegexOptions.IgnoreCase)

        let error = ref Unchecked.defaultof<ClErrorCode>

        Cl.GetPlatformIDs error
        |> Seq.choose
            (fun platform ->
                let platformName = Cl.GetPlatformInfo(platform, OpenCL.Net.PlatformInfo.Name, error).ToString()
                if platformNameRegex.Match(platformName).Success then
                    Some <| Cl.GetDeviceIDs(platform, DeviceHelpers.convertToDeviceType deviceType, error)
                else
                    None
            )
        |> Seq.concat
        |> Seq.map ClDevice

    static member GetFirstAppropriateDevice(?platform: Platform, ?deviceType: DeviceType) =
        let platform = defaultArg platform Platform.Any
        let deviceType = defaultArg deviceType DeviceType.Default

        try
            Seq.head <| ClDevice.GetAvailableDevices(platform, deviceType)
        with
        | :? System.ArgumentException as ex ->
            raise <| EmptyDevicesException $"No %A{deviceType} devices on platform %A{platform} were found"
