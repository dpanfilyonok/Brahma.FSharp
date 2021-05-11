module Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.MutableVarsToRef

open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils

open FSharp.Quotations

let rec mutableVarsToRefsImpl (refMap: Map<Var, Expr>) (expr: Expr) =
    match expr with
    | Patterns.LetVar (var, letExpr, body) ->
        if var.IsMutable
        then
            let refName = var.Name + "Ref"
            let refType = typedefof<ref<_>>.MakeGenericType(var.Type)
            let refVar = Var(refName, refType, false)

            let newRefMap =
                refMap.Add (var, Expr.Var(refVar))

            Expr.Let(var, mutableVarsToRefsImpl refMap letExpr,
                Expr.Let(
                    refVar, Common.createRefCall <| Expr.Var(var),
                    mutableVarsToRefsImpl newRefMap body
                )
            )
        else
            Expr.Let(
                var, mutableVarsToRefsImpl refMap letExpr,
                mutableVarsToRefsImpl refMap body
            )
    | Patterns.VarSet (var, valueExpr) ->
        match refMap.TryFind var with
        | Some refExpr ->
            Common.createReferenceSetCall refExpr <| mutableVarsToRefsImpl refMap valueExpr
        | None -> expr
    | ExprShape.ShapeVar var ->
        match refMap.TryFind var with
        | Some refExpr -> Common.createDereferenceCall refExpr
        | None -> expr
    | ExprShape.ShapeLambda (var, body) ->
        Expr.Lambda (var, mutableVarsToRefsImpl refMap body)
    | ExprShape.ShapeCombination (shapeComboObject, exprList) ->
        let exprList' = List.map (mutableVarsToRefsImpl refMap) exprList
        ExprShape.RebuildShapeCombination(shapeComboObject, exprList')

let mutableVarsToRefs (expr: Expr) =
    mutableVarsToRefsImpl Map.empty expr
