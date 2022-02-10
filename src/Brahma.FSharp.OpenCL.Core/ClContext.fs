namespace Brahma.FSharp.OpenCL

open OpenCL.Net

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

type ClContext(device) =
    let context =
        let error = ref Unchecked.defaultof<ErrorCode>
        let ctx = Cl.CreateContext(null, 1u, [| device |], null, System.IntPtr.Zero, error)

        if error.Value <> ErrorCode.Success then
            raise <| Cl.Exception error.Value

        ctx

    new(?platform: ClPlatform, ?deviceType: ClDeviceType) =
        let platform = defaultArg platform ClPlatform.Any
        let deviceType = defaultArg deviceType ClDeviceType.Default

        let device =
            Device.getFirstAppropriateDevice
            <| ClPlatform.ConvertToPattern platform
            <| ClDeviceType.ConvertToDeviceType deviceType

        ClContext(device)

    member this.Device = device

    member this.Context = context
