namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator

[<AutoOpen>]
module TransformerMinMax =
    let helper (expr: Expr) (type': System.Type) (x: Expr) (y: Expr) =
        let cachedXVar = Var("tempVarX", type')
        let cachedYVar = Var("tempVarY", type')

        Expr.Let(
            cachedXVar,
            x,
            Expr.Let(
                cachedYVar,
                y,
                Expr.IfThenElse(
                    Expr.Call(
                        Utils.makeGenericMethodCall [type'] expr,
                        [Expr.Var cachedXVar; Expr.Var cachedYVar]
                    ),
                    Expr.Var cachedXVar,
                    Expr.Var cachedYVar
                )
            )
        )

    let rec transformMinMax (expr: Expr) =
        match expr with
        | DerivedPatterns.SpecificCall <@@ max @@> (_, [genericParam], [x; y]) ->
            helper <@@ (>) @@> genericParam x y

        | DerivedPatterns.SpecificCall <@@ min @@> (_, [genericParam], [x; y]) ->
            helper <@@ (<) @@> genericParam x y

        | ExprShape.ShapeVar _ ->
            expr
        | ExprShape.ShapeLambda (x, body) ->
            Expr.Lambda(x, transformMinMax body)
        | ExprShape.ShapeCombination(combo, exprList) ->
            ExprShape.RebuildShapeCombination(combo, List.map transformMinMax exprList)
