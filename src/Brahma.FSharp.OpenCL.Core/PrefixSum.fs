namespace GraphBLAS.FSharp.Backend.Common

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Brahma.OpenCL

module Utils =
    let defaultWorkGroupSize = 32
    let getDefaultGlobalSize n =
        let m = n - 1
        m - m % defaultWorkGroupSize + defaultWorkGroupSize

module internal PrefixSum =

    open Brahma.FSharp.OpenCL
    open OpenCL.Net
    open Brahma.OpenCL

    let private getNewUpdate (gpu: GPU) =
        let workGroupSize = Utils.defaultWorkGroupSize

        let update =
            <@
                fun (ndRange: _1D)
                    inputArrayLength
                    bunchLength
                    (resultBuffer: int[])
                    (verticesBuffer: int[])
                     ->

                    let i = ndRange.GlobalID0 + bunchLength
                    if i < inputArrayLength then
                        resultBuffer.[i] <- resultBuffer.[i] + verticesBuffer.[i / bunchLength]
            @>

        let kernel = gpu.CreateKernel(update)

        fun (processor:MailboxProcessor<_>) (inputArray:GpuArray<int>) (inputArrayLength:int) (vertices:GpuArray<int>) (bunchLength: int) ->
            let ndRange = _1D(Utils.getDefaultGlobalSize inputArrayLength - bunchLength, workGroupSize)
            kernel.SetArguments ndRange inputArrayLength bunchLength inputArray vertices
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

    let private getNewScan (gpu:GPU) =

        let workGroupSize = Utils.defaultWorkGroupSize

        let scan =
            <@
                fun (ndRange: _1D)
                    inputArrayLength
                    verticesLength
                    (resultBuffer: int[])
                    (verticesBuffer: int[])
                    (totalSumBuffer: int[]) ->

                    let resultLocalBuffer = localArray<int> workGroupSize
                    let i = ndRange.GlobalID0
                    let localID = ndRange.LocalID0

                    if i < inputArrayLength then resultLocalBuffer.[localID] <- resultBuffer.[i] else resultLocalBuffer.[localID] <- 0

                    let mutable step = 2
                    while step <= workGroupSize do
                        barrier ()
                        if localID < workGroupSize / step then
                            let i = step * (localID + 1) - 1
                            resultLocalBuffer.[i] <- resultLocalBuffer.[i] + resultLocalBuffer.[i - (step >>> 1)]
                        step <- step <<< 1
                    barrier ()

                    if localID = workGroupSize - 1 then
                        if verticesLength <= 1 && localID = i then totalSumBuffer.[0] <- resultLocalBuffer.[localID]
                        verticesBuffer.[i / workGroupSize] <- resultLocalBuffer.[localID]
                        resultLocalBuffer.[localID] <- 0

                    step <- workGroupSize
                    while step > 1 do
                        barrier ()
                        if localID < workGroupSize / step then
                            let i = step * (localID + 1) - 1
                            let j = i - (step >>> 1)

                            let tmp = resultLocalBuffer.[i]
                            resultLocalBuffer.[i] <- resultLocalBuffer.[i] + resultLocalBuffer.[j]
                            resultLocalBuffer.[j] <- tmp
                        step <- step >>> 1
                    barrier ()

                    if i < inputArrayLength then resultBuffer.[i] <- resultLocalBuffer.[localID]
            @>

        let kernel = gpu.CreateKernel(scan)

        fun (processor:MailboxProcessor<_>) (inputArray: GpuArray<int>) (inputArrayLength: int) (vertices: GpuArray<int>) (verticesLength: int) (totalSum: GpuArray<int>) ->
            let ndRange = _1D(Utils.getDefaultGlobalSize inputArrayLength, workGroupSize)
            kernel.SetArguments ndRange inputArrayLength verticesLength inputArray vertices totalSum
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

    let runExcludeInplace (gpu:GPU) =

        let workGroupSize = Utils.defaultWorkGroupSize
        let scan = getNewScan gpu
        let update = getNewUpdate gpu

        fun (processor:MailboxProcessor<_>) (inputArray: GpuArray<int>) (totalSum: GpuArray<int>) ->
            let firstVertices = gpu.Allocate<int> <| (inputArray.Length - 1) / workGroupSize + 1
            let secondVertices = gpu.Allocate<int> <| (firstVertices.Length - 1) / workGroupSize + 1
            let mutable verticesArrays = firstVertices, secondVertices
            let swap (a, b) = (b, a)

            let mutable verticesLength = firstVertices.Length
            let mutable bunchLength = workGroupSize

            scan processor inputArray inputArray.Length (fst verticesArrays) verticesLength totalSum
            while verticesLength > 1 do
                let fstVertices = fst verticesArrays
                let sndVertices = snd verticesArrays
                scan processor fstVertices verticesLength sndVertices ((verticesLength - 1) / workGroupSize + 1) totalSum
                update processor inputArray inputArray.Length fstVertices bunchLength

                bunchLength <- bunchLength * workGroupSize
                verticesArrays <- swap verticesArrays
                verticesLength <- (verticesLength - 1) / workGroupSize + 1

            inputArray, totalSum



