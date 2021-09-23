namespace GraphBLAS.FSharp.Backend.COOMatrix.Utilities

open Brahma.FSharp.OpenCL
open OpenCL.Net
open GraphBLAS.FSharp.Backend.Common

type COOMatrix<'a> =
    {
        RowCount: int
        ColumnCount: int
        Rows: int[]
        Columns: int[]
        Values: 'a[]
    }

[<AutoOpen>]
module internal Merge =
    let merge<'a> (gpu:GPU)  =
        let workGroupSize = Utils.defaultWorkGroupSize

        let merge =
            <@
                fun (ndRange: _1D)
                    firstSide
                    secondSide
                    sumOfSides
                    (firstRowsBuffer: Buffer<int>)
                    (firstColumnsBuffer: Buffer<int>)
                    (firstValuesBuffer: Buffer<'a>)
                    (secondRowsBuffer: Buffer<int>)
                    (secondColumnsBuffer: Buffer<int>)
                    (secondValuesBuffer: Buffer<'a>)
                    (allRowsBuffer: Buffer<int>)
                    (allColumnsBuffer: Buffer<int>)
                    (allValuesBuffer: Buffer<'a>) ->

                    let i = ndRange.GlobalID0

                    let mutable beginIdxLocal = local ()
                    let mutable endIdxLocal = local ()
                    let localID = ndRange.LocalID0
                    if localID < 2 then
                        let mutable x = localID * (workGroupSize - 1) + i - 1
                        if x >= sumOfSides then x <- sumOfSides - 1
                        let diagonalNumber = x

                        let mutable leftEdge = diagonalNumber + 1 - secondSide
                        if leftEdge < 0 then leftEdge <- 0

                        let mutable rightEdge = firstSide - 1
                        if rightEdge > diagonalNumber then rightEdge <- diagonalNumber

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2
                            let firstIndex: uint64 = ((uint64 firstRowsBuffer.[middleIdx]) <<< 32) ||| (uint64 firstColumnsBuffer.[middleIdx])
                            let secondIndex: uint64 = ((uint64 secondRowsBuffer.[diagonalNumber - middleIdx]) <<< 32) ||| (uint64 secondColumnsBuffer.[diagonalNumber - middleIdx])
                            if firstIndex < secondIndex then leftEdge <- middleIdx + 1 else rightEdge <- middleIdx - 1

                        // Here localID equals either 0 or 1
                        if localID = 0 then beginIdxLocal <- leftEdge else endIdxLocal <- leftEdge
                    barrier ()

                    let beginIdx = beginIdxLocal
                    let endIdx = endIdxLocal
                    let firstLocalLength = endIdx - beginIdx
                    let mutable x = workGroupSize - firstLocalLength
                    if endIdx = firstSide then x <- secondSide - i + localID + beginIdx
                    let secondLocalLength = x

                    //First indices are from 0 to firstLocalLength - 1 inclusive
                    //Second indices are from firstLocalLength to firstLocalLength + secondLocalLength - 1 inclusive
                    let localIndices = localArray<uint64> workGroupSize

                    if localID < firstLocalLength then
                        localIndices.[localID] <- ((uint64 firstRowsBuffer.[beginIdx + localID]) <<< 32) ||| (uint64 firstColumnsBuffer.[beginIdx + localID])
                    if localID < secondLocalLength then
                        localIndices.[firstLocalLength + localID] <- ((uint64 secondRowsBuffer.[i - beginIdx]) <<< 32) ||| (uint64 secondColumnsBuffer.[i - beginIdx])
                    barrier ()

                    if i < sumOfSides then
                        let mutable leftEdge = localID + 1 - secondLocalLength
                        if leftEdge < 0 then leftEdge <- 0

                        let mutable rightEdge = firstLocalLength - 1
                        if rightEdge > localID then rightEdge <- localID

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2
                            let firstIndex = localIndices.[middleIdx]
                            let secondIndex = localIndices.[firstLocalLength + localID - middleIdx]
                            if firstIndex < secondIndex then leftEdge <- middleIdx + 1 else rightEdge <- middleIdx - 1

                        let boundaryX = rightEdge
                        let boundaryY = localID - leftEdge

                        // boundaryX and boundaryY can't be off the right edge of array (only off the left edge)
                        let isValidX = boundaryX >= 0
                        let isValidY = boundaryY >= 0

                        let mutable fstIdx = 0UL
                        if isValidX then fstIdx <- localIndices.[boundaryX]

                        let mutable sndIdx = 0UL
                        if isValidY then sndIdx <- localIndices.[firstLocalLength + boundaryY]

                        if not isValidX || isValidY && fstIdx < sndIdx then
                            allRowsBuffer.[i] <- int (sndIdx >>> 32)
                            allColumnsBuffer.[i] <- int sndIdx
                            allValuesBuffer.[i] <- secondValuesBuffer.[i - localID - beginIdx + boundaryY]
                        else
                            allRowsBuffer.[i] <- int (fstIdx >>> 32)
                            allColumnsBuffer.[i] <- int fstIdx
                            allValuesBuffer.[i] <- firstValuesBuffer.[beginIdx + boundaryX]
            @>

        let kernel = gpu.CreateKernel(merge)
        let sw = System.Diagnostics.Stopwatch()

        fun (processor:MailboxProcessor<_>)
            (matrixLeftRows: Buffer<int>) (matrixLeftColumns: Buffer<int>) (matrixLeftValues: Buffer<'a>)
            (matrixRightRows: Buffer<int>) (matrixRightColumns: Buffer<int>) (matrixRightValues: Buffer<'a>)
            ->

            //sw.Reset()
            let firstSide = matrixLeftValues.Length
            let secondSide = matrixRightValues.Length
            let sumOfSides = firstSide + secondSide

            //sw.Start()
            //printfn "1112"
            let allRows = gpu.Allocate<int>(sumOfSides, deviceAccessMode = DeviceAccessMode.WriteOnly, hostAccessMode = HostAccessMode.NotAccessible)
            let allColumns = gpu.Allocate<int>(sumOfSides, deviceAccessMode = DeviceAccessMode.WriteOnly, hostAccessMode = HostAccessMode.NotAccessible)
            let allValues = gpu.Allocate<'a>(sumOfSides, deviceAccessMode = DeviceAccessMode.WriteOnly, hostAccessMode = HostAccessMode.NotAccessible)
            //printfn "1113"

            //sw.Stop()
            //printfn "Data to gpu in Merge: %A" (sw.ElapsedMilliseconds)

            let ndRange = _1D(Utils.getDefaultGlobalSize sumOfSides, workGroupSize)

            processor.Post(Msg.MsgSetArguments(fun () -> 
                kernel.SetArguments
                    ndRange
                    firstSide
                    secondSide
                    sumOfSides
                    matrixLeftRows
                    matrixLeftColumns
                    matrixLeftValues
                    matrixRightRows
                    matrixRightColumns
                    matrixRightValues
                    allRows
                    allColumns
                    allValues
            ))
            processor.Post(Msg.CreateRunMsg<_,_>(kernel))

            allRows, allColumns, allValues
