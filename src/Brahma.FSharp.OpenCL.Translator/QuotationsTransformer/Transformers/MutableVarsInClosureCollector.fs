module Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.MutableVarsInClosureCollector

open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils
open FSharp.Quotations

let private isMutableVar (var: Var) =
    var.IsMutable && not (Common.isFunction var)

let rec collectMutableVarsInClosure (expr: Expr) =
    match expr with
    | Patterns.LetFunc (_, def, body) ->
        let mutableFreeVars = def |> Common.collectFreeVarsWithPredicate isMutableVar
        Set.unionMany [
            mutableFreeVars;
            collectMutableVarsInClosure def
            collectMutableVarsInClosure body
        ]
    | ExprShape.ShapeLambda (_, body) ->
        collectMutableVarsInClosure body
    | ExprShape.ShapeVar _ ->
        Set.empty
    | ExprShape.ShapeCombination(_, exprList) ->
        exprList
        |> List.map collectMutableVarsInClosure
        |> Set.unionMany
