namespace GraphBLAS.FSharp.Backend.COOMatrix

open Brahma.FSharp.OpenCL
open OpenCL.Net
//open Brahma.OpenCL
open GraphBLAS.FSharp.Backend.Common
open GraphBLAS.FSharp.Backend.COOMatrix.Utilities
open Microsoft.FSharp.Quotations

module internal rec EWiseAdd =
    let run (gpu:GPU) (matrixLeft: COOMatrix<'a>) (matrixRight: COOMatrix<'a>) (op: Expr<'a -> 'a -> 'a>) =
        if matrixLeft.Values.Length = 0 then
            let resultRows = matrixRight.Rows
            let resultColumns = matrixRight.Columns
            let resultValues = matrixRight.Values

            {
                RowCount = matrixRight.RowCount
                ColumnCount = matrixRight.ColumnCount
                Rows = resultRows
                Columns = resultColumns
                Values = resultValues
            }

        elif matrixRight.Values.Length = 0 then
            let resultRows = matrixLeft.Rows
            let resultColumns = matrixLeft.Columns
            let resultValues = matrixLeft.Values

            {
                RowCount = matrixLeft.RowCount
                ColumnCount = matrixLeft.ColumnCount
                Rows = resultRows
                Columns = resultColumns
                Values = resultValues
            }

        else
            let _do = gpuEWiseAdd gpu op
            _do matrixLeft matrixRight 

    let gpuEWiseAdd (gpu:GPU) (op: Expr<'a -> 'a -> 'a>) =

        let processor = gpu.GetNewProcessor ()

        let sw = System.Diagnostics.Stopwatch()
        let sw2 = System.Diagnostics.Stopwatch()
        let sw3 = System.Diagnostics.Stopwatch()
        let sw4 = System.Diagnostics.Stopwatch()

        sw4.Start()
        let merge = GraphBLAS.FSharp.Backend.COOMatrix.Utilities.Merge.merge gpu
        let preparePositions = GraphBLAS.FSharp.Backend.COOMatrix.Utilities.PreparePositions.preparePositions gpu op
        let setPositions = GraphBLAS.FSharp.Backend.COOMatrix.Utilities.SetPositions.setPositions<'a> gpu
        sw4.Stop()
        
        printfn "Compilation: %A" (sw4.ElapsedMilliseconds)

        fun (matrixLeft: COOMatrix<'a>) (matrixRight: COOMatrix<'a>) ->
            sw.Reset()
            sw2.Reset()
            sw3.Reset()
            //sw4.Reset()

            sw.Start()
            sw2.Start()

            use matrixLeftRows = gpu.Allocate<_>(matrixLeft.Rows, deviceAccessMode = DeviceAccessMode.ReadOnly)
            use matrixLeftColumns = gpu.Allocate<_>(matrixLeft.Columns, deviceAccessMode = DeviceAccessMode.ReadOnly)
            use matrixLeftValues = gpu.Allocate<'a>(matrixLeft.Values, deviceAccessMode = DeviceAccessMode.ReadOnly)
            use matrixRightRows = gpu.Allocate<_>(matrixRight.Rows, deviceAccessMode = DeviceAccessMode.ReadOnly)
            use matrixRightColumns = gpu.Allocate<_>(matrixRight.Columns, deviceAccessMode = DeviceAccessMode.ReadOnly)
            use matrixRightValues = gpu.Allocate<'a>(matrixRight.Values, deviceAccessMode = DeviceAccessMode.ReadOnly)

            sw2.Stop()

            let allRows, allColumns, allValues =
                merge
                    processor
                    matrixLeftRows matrixLeftColumns matrixLeftValues
                    matrixRightRows matrixRightColumns matrixRightValues

    
            use rawPositions = preparePositions processor allRows allColumns allValues

            let resultRows, resultColumns, resultValues, resultLength = setPositions processor allRows allColumns allValues rawPositions

            sw3.Start()

            let rows = Array.zeroCreate resultLength
            let columns = Array.zeroCreate resultLength
            let values = Array.zeroCreate resultLength
            processor.Post(Msg.CreateToHostMsg<int>(resultRows, rows))
            processor.Post(Msg.CreateToHostMsg<int>(resultColumns, columns))
            processor.PostAndReply(fun ch -> Msg.CreateToHostMsg<_>(resultValues, values, ch))

            sw3.Stop()
            sw.Stop()
            
            printfn "Data to gpu: %A" (sw2.ElapsedMilliseconds)
            printfn "Data to host: %A" (sw3.ElapsedMilliseconds)
            printfn "Elapsed total: %A" (sw.ElapsedMilliseconds)

            processor.Post(Msg.CreateFreeMsg<_>(resultRows))
            processor.Post(Msg.CreateFreeMsg<_>(resultColumns))
            processor.Post(Msg.CreateFreeMsg<_>(resultValues))
            processor.Post(Msg.CreateFreeMsg<_>(allRows))
            processor.Post(Msg.CreateFreeMsg<_>(allColumns))
            processor.Post(Msg.CreateFreeMsg<_>(allValues))

            {
                RowCount = matrixLeft.RowCount
                ColumnCount = matrixLeft.ColumnCount
                Rows = rows
                Columns = columns
                Values = values
            }
