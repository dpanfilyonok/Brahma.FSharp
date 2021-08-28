namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open GraphBLAS.FSharp.Backend.Common

[<AutoOpen>]
module internal PreparePositions =
    let preparePositions<'a> (gpu:GPU) (plus: Expr<'a -> 'a -> 'a>) =

        let preparePositions =
            <@
                fun (ndRange: _1D)
                    length
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (rawPositionsBuffer: int[]) ->

                    let i = ndRange.GlobalID0

                    if i < length - 1
                    && allRowsBuffer.[i] = allRowsBuffer.[i + 1]
                    && allColumnsBuffer.[i] = allColumnsBuffer.[i + 1]
                    then
                        rawPositionsBuffer.[i] <- 0
                        allValuesBuffer.[i + 1] <- (%plus) allValuesBuffer.[i] allValuesBuffer.[i + 1]

                        //Drop explicit zeroes
                        // let localResultBuffer = (%plus) allValuesBuffer.[i] allValuesBuffer.[i + 1]
                        // if localResultBuffer = zero then rawPositionsBuffer.[i + 1] <- 0 else allValuesBuffer.[i + 1] <- localResultBuffer
            @>

        let kernel = gpu.CreateKernel(preparePositions)

        fun (processor:MailboxProcessor<_>) (allRows: GpuArray<int>) (allColumns: GpuArray<int>) (allValues: GpuArray<'a>) ->
            let length = allValues.Length
            let ndRange = _1D(Utils.getDefaultGlobalSize (length - 1), Utils.defaultWorkGroupSize)

            let rawPositions = Array.create length 1
            let rawPositionsGpu = gpu.Allocate<_>(length)
            processor.Post(Msg.CreateToGPUMsg<_>(rawPositions, rawPositionsGpu))

            processor.Post(Msg.MsgSetArguments(fun () -> kernel.SetArguments ndRange length allRows allColumns allValues rawPositionsGpu))
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))
            rawPositionsGpu
