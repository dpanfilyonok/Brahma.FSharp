namespace Brahma.FSharp.OpenCL

open FSharp.Quotations
open OpenCL.Net

type ClContext(provider: ComputeProvider) =
    new(?platform: ClPlatform, ?deviceType: ClDeviceType) =
        let platform = defaultArg platform (ClPlatform.Pattern "Intel*")
        let deviceType = defaultArg deviceType ClDeviceType.CPU
        let device = (Device.getDevices "Intel*" DeviceType.Cpu).[0]
        ClContext(ComputeProvider device)

    member this.Provider = provider

type ClTask<'a> =
    ClTask of (ClContext -> 'a)

type ClTaskBuilder() =
    let runEvaluation (ClTask f) = f

    member this.Bind(m, k) = ClTask <| fun env ->
        let res = runEvaluation m env
        runEvaluation <| k res <| env

    member this.Return x = ClTask <| fun _ ->
        x

    member this.ReturnFrom m = m

    member this.Zero() =
        this.Return()

    member this.Combine(m1, m2) = ClTask <| fun env ->
        runEvaluation m1 env
        runEvaluation m2 env

    member this.Delay(rest) = ClTask <| fun env ->
        runEvaluation (rest()) env

    member this.While(predicate, body) = ClTask <| fun env ->
        while predicate() do
            runEvaluation body env

    member this.For(elems, body) = ClTask <| fun env ->
        for elem in elems do
            runEvaluation (body elem) env

    member this.TryWith(tryBlock, handler) = ClTask <| fun env ->
        try
            runEvaluation tryBlock env
        with
        | e ->
            runEvaluation (handler e) env

    member this.TryFinally(body, compensation) =
        try
            this.ReturnFrom(body())
        finally
            compensation()

    member this.Using(x: #System.IDisposable, f) =
        let body' = fun () -> f x
        this.TryFinally(body', fun () -> x.Dispose())

module ClTask =
    let runSync (context: ClContext) (ClTask f) =
        let res = f context
        context.Provider.CommandQueue.PostAndReply <| MsgFinish
        res

    let ask = ClTask id

[<AutoOpen>]
module ClTaskImpl =
    let opencl = ClTaskBuilder()

    let (>>=) x f = opencl.Bind(x, f)

    let runCommand (command: Expr<'range -> 'a>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
        opencl {
            let! ctx = ClTask.ask

            let kernel = ctx.Provider.CreateKernel command

            ctx.Provider.CommandQueue.Post <| MsgSetArguments(fun () -> binder kernel.SetArguments)
            ctx.Provider.CommandQueue.PostAndReply <| MsgNotifyMe
            ctx.Provider.CommandQueue.Post <| Msg.CreateRunMsg<_,_>(kernel)
        }
