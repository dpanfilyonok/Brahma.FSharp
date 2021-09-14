namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp.OpenCL
open OpenCL.Net
open GraphBLAS.FSharp.Backend.Common

[<AutoOpen>]
module internal SetPositions =
    let setPositions<'a> (gpu:GPU) =

        let setPositions =
            <@
                fun (ndRange: Brahma.OpenCL._1D)
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
        let resultLength = Array.zeroCreate 1
        let sw = System.Diagnostics.Stopwatch()

        fun (processor:MailboxProcessor<_>) (allRows: Buffer<int>) (allColumns: Buffer<int>) (allValues: Buffer<'a>) (positions: Buffer<int>) ->
            let prefixSumArrayLength = positions.Length            
            //printfn "4"
            let resultLengthGpu = gpu.Allocate<_>(1)
            //printfn "5"
            let _,r = sum processor positions resultLengthGpu
            //sw.Reset()
            //printfn "6"
            let resultLength =
                //printfn "7"
                let res = processor.PostAndReply(fun ch -> Msg.CreateToHostMsg<_>(r, resultLength, ch))
                match res with 
                | Error e -> raise e
                | Ok x ->  
                    //printfn "11"
                    processor.Post(Msg.CreateFreeMsg<_>(r))
                    x.[0]
            //printfn "8"
            //printfn "Result length = %A" resultLength
            //sw.Start()
            let resultRows = gpu.Allocate<int>(resultLength, hostAccessMode=HostAccessMode.NotAccessible, deviceAccessMode=DeviceAccessMode.WriteOnly)
            //printfn "#"
            let resultColumns = gpu.Allocate<int>(resultLength, hostAccessMode=HostAccessMode.NotAccessible, deviceAccessMode=DeviceAccessMode.WriteOnly)
            //printfn "##"
            let resultValues = gpu.Allocate<'a>(resultLength, hostAccessMode=HostAccessMode.NotAccessible, deviceAccessMode=DeviceAccessMode.WriteOnly)
            //printfn "9"
            //sw.Stop()
            //printfn "Data to gpu in SetPositions: %A" (sw.ElapsedMilliseconds)
            let ndRange = Brahma.OpenCL._1D(Utils.getDefaultGlobalSize positions.Length, Utils.defaultWorkGroupSize)
            processor.Post(Msg.MsgSetArguments( fun () ->    
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
            ))
            processor.Post(Msg.CreateRunMsg<_,_,_>(kernel))
            resultRows, resultColumns, resultValues, resultLength

