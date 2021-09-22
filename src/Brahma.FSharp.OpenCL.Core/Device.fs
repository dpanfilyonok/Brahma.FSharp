namespace Brahma.FSharp

open OpenCL.Net

[<RequireQualifiedAccess>]
type ClPlatform =
    | Intel
    | AMD
    | NVIDIA
    | Pattern of string

[<RequireQualifiedAccess>]
type ClDeviceType =
    | CPU
    | GPU
    | Default

module Device =
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
