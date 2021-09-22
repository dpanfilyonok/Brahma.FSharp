namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp
open OpenCL.Net
open Microsoft.FSharp.Quotations
open GraphBLAS.FSharp.Backend.Common

[<AutoOpen>]
module internal PreparePositions =
    let preparePositions
        (allRows: ClArray<int>)
        (allColumns: ClArray<int>)
        (allValues: ClArray<'a>)
        (plus: Expr<'a -> 'a -> 'a>) = opencl {

        let preparePositions =
            <@
                fun (ndRange: Range1D)
                    length
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (rawPositionsBuffer: int[]) ->

                    let i = ndRange.GlobalID0

                    if
                        i < length - 1 &&
                        allRowsBuffer.[i] = allRowsBuffer.[i + 1] &&
                        allColumnsBuffer.[i] = allColumnsBuffer.[i + 1]
                    then
                        rawPositionsBuffer.[i] <- 0
                        allValuesBuffer.[i + 1] <- (%plus) allValuesBuffer.[i] allValuesBuffer.[i + 1]
                    else
                        rawPositionsBuffer.[i] <- 1 //TODO is it correct?
            @>

        let length = allValues.Length
        let ndRange = Range1D(Utils.getDefaultGlobalSize (length - 1), Utils.defaultWorkGroupSize)
        let! rawPositionsGpu = ClArray.alloc<int> length

        do! runCommand preparePositions <| fun aaa ->
            aaa ndRange length allRows allColumns allValues rawPositionsGpu

        return rawPositionsGpu
    }
