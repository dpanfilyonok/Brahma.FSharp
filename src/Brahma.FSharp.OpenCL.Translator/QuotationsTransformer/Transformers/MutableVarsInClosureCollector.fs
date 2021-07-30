namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations

[<AutoOpen>]
module MutableVarsInClosureCollector =
    let private isMutableVar (var: Var) =
        var.IsMutable && not (Utils.isFunction var)

    let rec collectMutableVarsInClosure (expr: Expr) =
        match expr with
        | Patterns.LetFunc (_, body, inExpr) ->
            let mutableFreeVars = body |> Utils.collectFreeVarsWithPredicate isMutableVar
            Set.unionMany [
                mutableFreeVars
                collectMutableVarsInClosure body
                collectMutableVarsInClosure inExpr
            ]
        | ExprShape.ShapeLambda (_, body) ->
            collectMutableVarsInClosure body
        | ExprShape.ShapeVar _ ->
            Set.empty
        | ExprShape.ShapeCombination(_, exprList) ->
            exprList
            |> List.map collectMutableVarsInClosure
            |> Set.unionMany
