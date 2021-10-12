namespace Brahma.FSharp.OpenCL

open FSharp.Quotations

type ClTask<'a> = ClTask of (ClContext -> 'a)

type ClTaskBuilder() =
    let runComputation (ClTask f) env = f env

    member this.Bind(x, f) =
        ClTask <| fun env ->
            let x' = runComputation x env
            runComputation (f x') env

    member this.Return(x) =
        ClTask <| fun _ ->
            x

    member this.ReturnFrom(x) =
        x

    member this.Zero() =
        this.Return(())

    member this.Combine(m1, m2) =
        this.Bind(m1, (fun () -> m2))

    member this.Delay(rest) =
        this.Bind(this.Zero(), (fun () -> rest ()))

    member this.Run(m) = m

    member this.TryWith(ClTask body, handler) =
        ClTask <| fun env ->
            try
                body env
            with
            | e ->
                let (ClTask handlerBody) = handler e
                handlerBody env

    member this.TryFinally(ClTask body, finalizer) =
        ClTask <| fun env ->
            try
                body env
            finally
                finalizer ()

    member this.Using(disposableRes: #System.IDisposable, f) =
        this.TryFinally(
            this.Delay(fun () -> f disposableRes),
            fun () -> disposableRes.Dispose()
        )

    member this.While(cond, body) =
        if not (cond ()) then
            this.Zero()
        else
            this.Combine(this.Run(body), this.Delay(fun () -> this.While(cond, body)))

    member this.For(xs: seq<'T>, f) =
        this.Bind(
            this.Return(xs.GetEnumerator()),
            fun en -> this.While((fun () -> en.MoveNext()), this.Delay(fun () -> f en.Current))
        )

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

            let kernel = ctx.CreateKernel command

            ctx.Provider.CommandQueue.Post <| MsgSetArguments(fun () -> binder kernel.SetArguments)
            ctx.Provider.CommandQueue.Post <| Msg.CreateRunMsg<_, _>(kernel)
        }

    let runKernel (kernel: ClKernel<'range, 'a>) (processor: MailboxProcessor<Msg>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
        opencl {
            processor.Post <| MsgSetArguments(fun () -> binder kernel.SetArguments)
            processor.Post <| Msg.CreateRunMsg<_, _>(kernel)
        }
