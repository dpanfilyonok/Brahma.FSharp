namespace Brahma.FSharp.OpenCL.WorkflowBuilder

open Brahma.OpenCL
open OpenCL.Net

exception EmptyDevicesException of string

type OpenCLEvaluationContext(provider: ComputeProvider, ?deviceId: int) =
    let devices = provider.Devices |> Seq.toArray
    let deviceId = defaultArg deviceId 0
    do
        if deviceId >= devices.Length then
            raise <| EmptyDevicesException (sprintf "Provider:\n%Ahas not device with id %i." provider deviceId)

    let device = devices.[deviceId]
    let commandQueue = new Brahma.OpenCL.CommandQueue(provider, device)

    new(?platformName, ?deviceType: DeviceType) =
        let platformName = defaultArg platformName "*"
        let deviceType = defaultArg deviceType DeviceType.Default
        let provider = ComputeProvider.Create(platformName, deviceType)
        OpenCLEvaluationContext(provider)

    member this.Provider = provider
    member this.Device = device
    member this.CommandQueue = commandQueue

    override this.ToString() =
        let mutable e = ErrorCode.Unknown
        let deviceName = Cl.GetDeviceInfo(this.Device, DeviceInfo.Name, &e).ToString()
        if deviceName.Length < 20 then
            sprintf "%s" deviceName
        else
            let platform = Cl.GetDeviceInfo(this.Device, DeviceInfo.Platform, &e).CastTo<Platform>()
            let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, &e).ToString()
            let deviceType =
                match Cl.GetDeviceInfo(this.Device, DeviceInfo.Type, &e).CastTo<DeviceType>() with
                | DeviceType.Cpu -> "CPU"
                | DeviceType.Gpu -> "GPU"
                | DeviceType.Accelerator -> "Accelerator"
                | _ -> "another"

            sprintf "%s, %s" platformName deviceType

type OpenCLEvaluation<'a> =
    OpenCLEvaluation of (OpenCLEvaluationContext -> 'a)

type OpenCLEvaluationBuilder() =
    let runEvaluation (OpenCLEvaluation f) = f

    abstract member Return : 'a -> OpenCLEvaluation<'a>

    abstract member ReturnFrom : OpenCLEvaluation<'a> -> OpenCLEvaluation<'a>

    abstract member Zero : unit -> OpenCLEvaluation<unit>

    abstract member Bind : OpenCLEvaluation<'a> * ('a -> OpenCLEvaluation<'b>) -> OpenCLEvaluation<'b>

    abstract member Combine : OpenCLEvaluation<unit> * OpenCLEvaluation<'a> -> OpenCLEvaluation<'a>

    abstract member Delay : (unit -> OpenCLEvaluation<'a>) -> OpenCLEvaluation<'a>

    abstract member While : (unit -> bool) * OpenCLEvaluation<unit> -> OpenCLEvaluation<unit>

    abstract member For : seq<'elem> * ('elem -> OpenCLEvaluation<unit>) -> OpenCLEvaluation<unit>

    abstract member TryWith : OpenCLEvaluation<'a> * (exn -> OpenCLEvaluation<'a>) -> OpenCLEvaluation<'a>

    default this.Return x =
        OpenCLEvaluation <| fun _ -> x

    default this.ReturnFrom m = m

    default this.Zero () =
        this.Return ()

    default this.Bind (m, k) =
        OpenCLEvaluation <| fun env ->
            let res = runEvaluation m env
            runEvaluation <| k res <| env

    default this.Combine (m1, m2) =
        OpenCLEvaluation <| fun env ->
            runEvaluation m1 env
            runEvaluation m2 env

    default this.Delay (rest) =
        OpenCLEvaluation <| fun env ->
            runEvaluation (rest()) env

    default this.While (predicate, body) =
        OpenCLEvaluation <| fun env ->
            while predicate() do
                runEvaluation body env

    default this.For (elems, body) =
        OpenCLEvaluation <| fun env ->
            for elem in elems do
                runEvaluation (body elem) env

    default this.TryWith (tryBlock, handler) =
        OpenCLEvaluation <| fun env ->
            try
                runEvaluation tryBlock env
            with
            | e ->
                runEvaluation (handler e) env
