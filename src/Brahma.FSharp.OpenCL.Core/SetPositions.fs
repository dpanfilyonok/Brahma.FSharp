namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Brahma.OpenCL
open GraphBLAS.FSharp.Backend.Common

[<AutoOpen>]
module internal SetPositions =
    let setPositions<'a> (gpu:GPU) =

        let setPositions =
            <@
                fun (ndRange: _1D)
                    prefixSumArrayLength
                    (allRowsBuffer: int[])
                    (allColumnsBuffer: int[])
                    (allValuesBuffer: 'a[])
                    (prefixSumArrayBuffer: int[])
                    (resultRowsBuffer: int[])
                    (resultColumnsBuffer: int[])
                    (resultValuesBuffer: 'a[]) ->

                    let i = ndRange.GlobalID0

                    if i = prefixSumArrayLength - 1
                    || i < prefixSumArrayLength
                    && prefixSumArrayBuffer.[i] <> prefixSumArrayBuffer.[i + 1]
                    then
                        let index = prefixSumArrayBuffer.[i]

                        resultRowsBuffer.[index] <- allRowsBuffer.[i]
                        resultColumnsBuffer.[index] <- allColumnsBuffer.[i]
                        resultValuesBuffer.[index] <- allValuesBuffer.[i]
            @>

        let kernel = gpu.CreateKernel(setPositions)
        let sum = PrefixSum.runExcludeInplace gpu

        fun (processor:MailboxProcessor<_>) (allRows: GpuArray<int>) (allColumns: GpuArray<int>) (allValues: GpuArray<'a>) (positions: GpuArray<int>) ->

            let prefixSumArrayLength = positions.Length
            let resultLength = Array.zeroCreate 1
            let resultLengthGpu = gpu.Allocate<_>(1)

            let _,r = sum processor positions resultLengthGpu

            let resultLength =
                processor.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(r, resultLength, ch)))
                resultLength.[0]

            let resultRows = gpu.Allocate<int>(resultLength)
            let resultColumns = gpu.Allocate<int>(resultLength)
            let resultValues = gpu.Allocate<'a>(resultLength)

            let ndRange = _1D(Utils.getDefaultGlobalSize positions.Length, Utils.defaultWorkGroupSize)

            kernel.SetArguments
                    ndRange
                    prefixSumArrayLength
                    allRows
                    allColumns
                    allValues
                    positions
                    resultRows
                    resultColumns
                    resultValues

            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

            resultRows, resultColumns, resultValues, resultLength

