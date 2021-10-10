namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open FSharp.Quotations

exception EmptyDevicesException of string

[<RequireQualifiedAccess>]
type ClPlatform =
    | Intel
    | AMD
    | NVIDIA
    | Pattern of string

    static member internal ConvertToPattern(platform: ClPlatform) =
        match platform with
        | Intel -> "Intel*"
        | AMD -> "AMD*"
        | NVIDIA -> "NVIDIA*"
        | Pattern pattern -> pattern

[<RequireQualifiedAccess>]
type ClDeviceType =
    | CPU
    | GPU
    | Any

    static member internal ConvertToDeviceType(deviceType: ClDeviceType) =
        match deviceType with
        | CPU -> DeviceType.Cpu
        | GPU -> DeviceType.Gpu
        | Any -> DeviceType.Default

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
        with _ ->
            raise <| EmptyDevicesException ""

type ClContext(provider: ComputeProvider) =
    new(?platform: ClPlatform, ?deviceType: ClDeviceType) =
        let platform = defaultArg platform (ClPlatform.Pattern "*")
        let deviceType = defaultArg deviceType ClDeviceType.Any

        let device =
            Device.getFirstAppropriateDevice
            <| ClPlatform.ConvertToPattern platform
            <| ClDeviceType.ConvertToDeviceType deviceType

        ClContext(ComputeProvider device)

    member this.Provider = provider

    member this.CreateKernel(srcLambda: Expr<'a -> 'b>) = ClKernel<_,_>(provider, srcLambda)

