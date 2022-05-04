namespace Brahma.FSharp

open FSharp.Quotations

[<AutoOpen>]
module ClContextExtensions =
    type ClContext with
        member this.Compile(srcLambda: Expr<'TRange ->'a>) = ClProgram(this, srcLambda)

        member this.CreateClArray
            (
                data: 'a[],
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Data data, flags)
            new ClArray<_>(buffer)

        member this.CreateClArray
            (
                size: int,
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Size size, flags)
            new ClArray<_>(buffer)

        member this.CreateClCell
            (
                data: 'a,
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Data [| data |], flags)
            new ClCell<_>(buffer)

        member this.CreateClCell
            (
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Size 1, flags)
            new ClCell<_>(buffer)
