namespace GraphBLAS.FSharp.Backend.COOMatrix

open Brahma.FSharp
open OpenCL.Net
open GraphBLAS.FSharp.Backend.Common
open GraphBLAS.FSharp.Backend.COOMatrix.Utilities
open Microsoft.FSharp.Quotations

module internal rec EWiseAdd =
    let run (matrixLeft: COOMatrix<'a>) (matrixRight: COOMatrix<'a>) (op: Expr<'a -> 'a -> 'a>) = opencl {
        if matrixLeft.Values.Length = 0 then
            let resultRows = matrixRight.Rows
            let resultColumns = matrixRight.Columns
            let resultValues = matrixRight.Values

            return {
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

            return {
                RowCount = matrixLeft.RowCount
                ColumnCount = matrixLeft.ColumnCount
                Rows = resultRows
                Columns = resultColumns
                Values = resultValues
            }

        else
            use! a1 = ClArray.toDevice matrixLeft.Rows
            use! b1 = ClArray.toDevice matrixLeft.Columns
            use! c1 = ClArray.toDevice matrixLeft.Values
            use! a2 = ClArray.toDevice matrixRight.Rows
            use! b2 = ClArray.toDevice matrixRight.Columns
            use! c2 = ClArray.toDevice matrixRight.Values

            // TODO use!
            let! (allRows, allColumns, allValues) = merge a1 b1 c1 a2 b2 c2
            let! rawPositions = preparePositions allRows allColumns allValues op
            let! (resultRows, resultColumns, resultValues, _) = setPositions allRows allColumns allValues rawPositions

            return {
                RowCount = matrixLeft.RowCount
                ColumnCount = matrixLeft.ColumnCount
                Rows = resultRows
                Columns = resultColumns
                Values = resultValues
            }
    }
