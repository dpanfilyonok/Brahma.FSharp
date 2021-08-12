module Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.MutableVarsToRef

open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils

open FSharp.Quotations

let rec varsToRefsWithPredicateImpl (refMap: Map<Var, Expr>) (predicate: Var -> bool) (expr: Expr) =
    match expr with
    | Patterns.LetVar (var, letExpr, body) ->
        if predicate var
        then
            let refName = var.Name + "Ref"
            let refType = typedefof<ref<_>>.MakeGenericType(var.Type)
            let refVar = Var(refName, refType, false)

            let newRefMap =
                refMap.Add (var, Expr.Var(refVar))

            Expr.Let(var, varsToRefsWithPredicateImpl refMap predicate letExpr,
                Expr.Let(
                    refVar, Common.createRefCall <| Expr.Var(var),
                    varsToRefsWithPredicateImpl newRefMap predicate body
                )
            )
        else
            Expr.Let(
                var, varsToRefsWithPredicateImpl refMap predicate letExpr,
                varsToRefsWithPredicateImpl refMap predicate body
            )
    | Patterns.VarSet (var, valueExpr) ->
        match refMap.TryFind var with
        | Some refExpr ->
            Common.createReferenceSetCall refExpr <| varsToRefsWithPredicateImpl refMap predicate valueExpr
        | None -> expr
    | ExprShape.ShapeVar var ->
        match refMap.TryFind var with
        | Some refExpr -> Common.createDereferenceCall refExpr
        | None -> expr
    | ExprShape.ShapeLambda (var, body) ->
        Expr.Lambda (var, varsToRefsWithPredicateImpl refMap predicate body)
    | ExprShape.ShapeCombination (shapeComboObject, exprList) ->
        let exprList' = List.map (varsToRefsWithPredicateImpl refMap predicate) exprList
        ExprShape.RebuildShapeCombination(shapeComboObject, exprList')

let varsToRefsWithPredicate (predicate: Var -> bool) (expr: Expr) =
    varsToRefsWithPredicateImpl Map.empty predicate expr
