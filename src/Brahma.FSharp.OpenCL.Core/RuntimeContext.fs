namespace Brahma.FSharp

type RuntimeOptions =
    {
        // TODO if 2D or 3D
        WorkgroupSize: int
    }

    static member Default =
        {
            WorkgroupSize = 256
        }

type RuntimeContext(clContext: ClContext, ?runtimeOptions: RuntimeOptions) =
    let runtimeOptions = defaultArg runtimeOptions RuntimeOptions.Default

    let mutable queue = clContext.QueueProvider.CreateQueue()

    member this.CommandQueue
        with get() = queue
        and private set(value) = queue <- value

    member this.ClContext = clContext

    member this.RuntimeOptions = runtimeOptions

    member internal this.WithNewCommandQueue() =
        RuntimeContext(clContext, runtimeOptions)

    member internal this.WithRuntimeOptions(runtimeOptions) =
        RuntimeContext(clContext, runtimeOptions, CommandQueue = this.CommandQueue)

    override this.ToString() = clContext.ToString()
