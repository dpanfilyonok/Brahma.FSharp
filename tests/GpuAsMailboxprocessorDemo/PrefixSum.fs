namespace GraphBLAS.FSharp.Backend.Common

open Brahma.FSharp
open OpenCL.Net

module Utils =
    let defaultWorkGroupSize = 256
    let getDefaultGlobalSize n =
        let m = n - 1
        m - m % defaultWorkGroupSize + defaultWorkGroupSize

module internal PrefixSum =
    let private getNewUpdate (inputArray: ClArray<int>) (inputArrayLength:int) (vertices:ClArray<int>) (bunchLength: int) = opencl {
        let workGroupSize = Utils.defaultWorkGroupSize

        let update =
            <@
                fun (ndRange: Range1D)
                    inputArrayLength
                    bunchLength
                    (resultBuffer: int[])
                    (verticesBuffer: int[]) ->

                    let i = ndRange.GlobalID0 + bunchLength
                    if i < inputArrayLength then
                        resultBuffer.[i] <- resultBuffer.[i] + verticesBuffer.[i / bunchLength]
            @>


        let ndRange = Range1D(Utils.getDefaultGlobalSize inputArrayLength - bunchLength, workGroupSize)
        do! runCommand update <| fun a -> a ndRange inputArrayLength bunchLength inputArray vertices
    }

    let private getNewScan (inputArray: ClArray<int>) (inputArrayLength: int) (vertices: ClArray<int>) (verticesLength: int) (totalSum: ClArray<int>) = opencl {
        let workGroupSize = Utils.defaultWorkGroupSize

        let scan =
            <@
                fun (ndRange: Range1D)
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

        let ndRange = Range1D(Utils.getDefaultGlobalSize inputArrayLength, workGroupSize)
        do! runCommand scan <| fun a -> a ndRange inputArrayLength verticesLength inputArray vertices totalSum
    }

    let runExcludeInplace (inputArray: ClArray<int>) (totalSum: ClArray<int>) = opencl {
        let workGroupSize = Utils.defaultWorkGroupSize

        use! firstVertices = ClArray.alloc<int> ((inputArray.Length - 1) / workGroupSize + 1)
        use! secondVertices = ClArray.alloc<int> ((firstVertices.Length - 1) / workGroupSize + 1)

        let mutable verticesArrays = firstVertices, secondVertices
        let swap (a, b) = (b, a)

        let mutable verticesLength = firstVertices.Length
        let mutable bunchLength = workGroupSize

        do! getNewScan inputArray inputArray.Length (fst verticesArrays) verticesLength totalSum
        while verticesLength > 1 do
            let fstVertices = fst verticesArrays
            let sndVertices = snd verticesArrays
            do! getNewScan fstVertices verticesLength sndVertices ((verticesLength - 1) / workGroupSize + 1) totalSum
            do! getNewUpdate inputArray inputArray.Length fstVertices bunchLength
            bunchLength <- bunchLength * workGroupSize
            verticesArrays <- swap verticesArrays
            verticesLength <- (verticesLength - 1) / workGroupSize + 1

        return inputArray, totalSum
    }


