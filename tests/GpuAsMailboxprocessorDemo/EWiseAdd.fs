namespace GraphBLAS.FSharp.Backend.COOMatrix

open Brahma.FSharp
open GraphBLAS.FSharp.Backend.COOMatrix.Utilities
open Microsoft.FSharp.Quotations

type COOMatrix<'a when 'a : struct> =
    {
        RowCount: int
        ColumnCount: int
        Rows: int[]
        Columns: int[]
        Values: 'a[]
    }

    member this.ToDevice() =
        opencl {
            let! a = ClArray.toDevice this.Rows
            let! b = ClArray.toDevice this.Columns
            let! c = ClArray.toDevice this.Values

            return {
                RowCount = this.RowCount
                ColumnCount = this.ColumnCount
                ClRows = a
                ClColumns = b
                ClValues = c
            }
        }

and ClCOOMatrix<'a when 'a : struct> =
    {
        RowCount: int
        ColumnCount: int
        ClRows: int clarray
        ClColumns: int clarray
        ClValues: 'a clarray
    }

    member this.ToHost() =
        opencl {
            let! a = ClArray.toHost this.ClRows
            let! b = ClArray.toHost this.ClColumns
            let! c = ClArray.toHost this.ClValues

            return {
                RowCount = this.RowCount
                ColumnCount = this.ColumnCount
                Rows = a
                Columns = b
                Values = c
            }
        }

    interface System.IDisposable with
        member this.Dispose() =
            this.ClRows.Dispose()
            this.ClColumns.Dispose()
            this.ClValues.Dispose()

module internal EWiseAdd =
    let run (matrixLeft: ClCOOMatrix<'a>) (matrixRight: ClCOOMatrix<'a>) (op: Expr<'a -> 'a -> 'a>) = opencl {
        if matrixLeft.ClValues.Length = 0 then
            let! resultRows = ClArray.copy matrixRight.ClRows
            let! resultColumns = ClArray.copy matrixRight.ClColumns
            let! resultValues = ClArray.copy matrixRight.ClValues

            return {
                RowCount = matrixRight.RowCount
                ColumnCount = matrixRight.ColumnCount
                ClRows = resultRows
                ClColumns = resultColumns
                ClValues = resultValues
            }

        elif matrixRight.ClValues.Length = 0 then
            let! resultRows = ClArray.copy matrixLeft.ClRows
            let! resultColumns = ClArray.copy matrixLeft.ClColumns
            let! resultValues = ClArray.copy matrixLeft.ClValues

            return {
                RowCount = matrixLeft.RowCount
                ColumnCount = matrixLeft.ColumnCount
                ClRows = resultRows
                ClColumns = resultColumns
                ClValues = resultValues
            }

        else
            // TODO use!
            let! (allRows, allColumns, allValues) =
                merge matrixLeft.ClRows matrixLeft.ClColumns matrixLeft.ClValues matrixRight.ClRows matrixRight.ClColumns matrixRight.ClValues

            let! rawPositions = preparePositions allRows allColumns allValues op
            let! (resultRows, resultColumns, resultValues, _) = setPositions allRows allColumns allValues rawPositions

            return {
                RowCount = matrixLeft.RowCount
                ColumnCount = matrixLeft.ColumnCount
                ClRows = resultRows
                ClColumns = resultColumns
                ClValues = resultValues
            }
    }
