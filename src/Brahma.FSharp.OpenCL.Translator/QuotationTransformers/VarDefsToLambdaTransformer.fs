namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open FSharp.Reflection
open Microsoft.FSharp.Core.LanguagePrimitives

[<AutoOpen>]
module VarDefsToLambdaTransformer =
    let rec isPrimitiveExpression (expr: Expr) =
        match expr with
        | Patterns.Value _
        | Patterns.ValueWithName _
        | Patterns.DefaultValue _
        | Patterns.Var _ -> true
        | Patterns.Call (_, _, args) -> List.forall isPrimitiveExpression args
        | Patterns.FieldGet (instance, _) ->
            instance
            |> Option.map isPrimitiveExpression
            |> Option.defaultValue true
        | Patterns.PropertyGet (instance, _, args) ->
            let isPrimitiveInstance =
                instance
                |> Option.map isPrimitiveExpression
                |> Option.defaultValue true

            let isPrimitiveArgs = List.forall isPrimitiveExpression args
            isPrimitiveInstance && isPrimitiveArgs
        | Patterns.NewUnionCase _ -> true
        | _ -> false

    // let x = expr -> let x = let unit () = expr in unit ()
    let rec transformVarDefsToLambda (expr: Expr) =
        match expr with
        | Patterns.LetVar (var, body, inExpr) ->
            if isPrimitiveExpression body then
                Expr.Let(var, body, transformVarDefsToLambda inExpr)
            else
                let fType = FSharpType.MakeFunctionType(typeof<unit>, var.Type)
                let fVar = Var(var.Name + "UnitFunc", fType)

                Expr.Let(
                    var,
                    Expr.Let(
                        fVar,
                        Expr.Lambda(Var("unitVar", typeof<unit>), transformVarDefsToLambda body),
                        Expr.Application(Expr.Var fVar, Expr.Value((), typeof<unit>))
                    ),
                    transformVarDefsToLambda inExpr
                )

        | ExprShape.ShapeVar _ -> expr
        | ExprShape.ShapeLambda (var, body) -> Expr.Lambda(var, transformVarDefsToLambda body)
        | ExprShape.ShapeCombination (shapeComboObject, exprList) ->
            let exprList' = List.map transformVarDefsToLambda exprList
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList')
