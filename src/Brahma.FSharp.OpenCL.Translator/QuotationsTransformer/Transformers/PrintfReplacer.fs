module Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.PrintfReplacer

open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils.Patterns
open FSharp.Quotations


/// Function for replacing printf call
let print (tpArgs: System.Type list) (value: string) (bindArgs: Expr list) = ()

let rec replacePrintf (expr: Expr) =
    match expr with
    | Printf (tpArgs, value, bindArgs) ->
        <@@
            print tpArgs value bindArgs
        @@>
    | ExprShape.ShapeVar _ ->
        expr
    | ExprShape.ShapeLambda (x, body) ->
        Expr.Lambda(x, replacePrintf body)
    | ExprShape.ShapeCombination(combo, exprList) ->
        ExprShape.RebuildShapeCombination(combo, List.map replacePrintf exprList)
