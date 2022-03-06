namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type ClContext(clDevice: ClDevice) =
    let context =
        let error = ref Unchecked.defaultof<ErrorCode>
        let ctx = Cl.CreateContext(null, 1u, [| clDevice.Device |], null, System.IntPtr.Zero, error)

        if error.Value <> ErrorCode.Success then
            raise <| Cl.Exception error.Value

        ctx

    member this.ClDevice = clDevice

    member this.Context = context
