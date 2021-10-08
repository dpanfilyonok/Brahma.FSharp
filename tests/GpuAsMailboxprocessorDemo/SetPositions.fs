namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp
open OpenCL.Net
open GraphBLAS.FSharp.Backend.Common

[<AutoOpen>]
module internal SetPositions =
    let setPositions
        (allRows: ClArray<int>)
        (allColumns: ClArray<int>)
        (allValues: ClArray<'a>)
        (positions: ClArray<int>) = opencl {

        let setPositions =
            <@
                fun (ndRange: Range1D)
                    prefixSumArrayLength
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (prefixSumArrayBuffer: int[])
                    (resultRowsBuffer: int[])
                    (resultColumnsBuffer: int[])
                    (resultValuesBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0

                    if
                        i = prefixSumArrayLength - 1 ||
                        i < prefixSumArrayLength &&
                        prefixSumArrayBuffer.[i] <> prefixSumArrayBuffer.[i + 1]
                    then
                        let index = prefixSumArrayBuffer.[i]
                        resultRowsBuffer.[index] <- allRowsBuffer.[i]
                        resultColumnsBuffer.[index] <- allColumnsBuffer.[i]
                        resultValuesBuffer.[index] <- allValuesBuffer.[i]
            @>

        use! resultLengthGpu = ClArray.alloc<int> 1
        let! _,r = PrefixSum.runExcludeInplace positions resultLengthGpu
        let prefixSumArrayLength = positions.Length
        let! resLen = ClArray.toHost r
        let resLen = resLen.[0]

        let! resultRows = ClArray.alloc<int> resLen
        let! resultColumns = ClArray.alloc<int> resLen
        let! resultValues = ClArray.alloc<'a> resLen

        let ndRange = Range1D(Utils.getDefaultGlobalSize positions.Length, Utils.defaultWorkGroupSize)
        do! runCommand setPositions <| fun x ->
            x
                ndRange
                prefixSumArrayLength
                allRows
                allColumns
                allValues
                positions
                resultRows
                resultColumns
                resultValues
        return resultRows, resultColumns, resultValues, resLen
    }
