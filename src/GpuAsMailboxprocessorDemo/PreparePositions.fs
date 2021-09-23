namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Microsoft.FSharp.Quotations
open GraphBLAS.FSharp.Backend.Common

[<AutoOpen>]
module internal PreparePositions =
    let preparePositions<'a> (gpu:GPU) (plus: Expr<'a -> 'a -> 'a>) =

        let preparePositions =
            <@
                fun (ndRange: _1D)
                    length
                    (allRowsBuffer: Buffer<int>)
                    (allColumnsBuffer: Buffer<int>)
                    (allValuesBuffer: Buffer<'a>)
                    (rawPositionsBuffer: Buffer<int>) ->

                    let i = ndRange.GlobalID0

                    if (i < length - 1
                    && allRowsBuffer.[i] = allRowsBuffer.[i + 1]
                    && allColumnsBuffer.[i] = allColumnsBuffer.[i + 1])
                    then
                        rawPositionsBuffer.[i] <- 0
                        allValuesBuffer.[i + 1] <- (%plus) allValuesBuffer.[i] allValuesBuffer.[i + 1]
                    else 
                        (rawPositionsBuffer.[i] <- 1) //TODO is it correct?                        
            @>

        let kernel = gpu.CreateKernel(preparePositions)

        fun (processor:MailboxProcessor<_>) (allRows: Buffer<int>) (allColumns: Buffer<int>) (allValues: Buffer<'a>) ->
            let length = allValues.Length
            let ndRange = _1D(Utils.getDefaultGlobalSize (length - 1), Utils.defaultWorkGroupSize)
            //printfn "2"
            let rawPositionsGpu = gpu.Allocate<int>(length, hostAccessMode = HostAccessMode.NotAccessible)
            //printfn "3"
            processor.Post(Msg.MsgSetArguments(fun () -> kernel.SetArguments ndRange length allRows allColumns allValues rawPositionsGpu))
            processor.Post(Msg.CreateRunMsg<_,_>(kernel))
            rawPositionsGpu
