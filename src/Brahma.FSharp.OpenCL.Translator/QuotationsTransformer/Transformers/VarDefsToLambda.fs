namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations
open FSharp.Reflection

module LetVarAbstracter =
    let rec isPrimitiveExpression (expr: Expr) =
        match expr with
        | Patterns.Value _ | Patterns.ValueWithName _ | Patterns.Var _ -> true
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

    // let x = expr -> let x = let init () = expr in init ()
    let rec varDefsToLambda (expr: Expr) =
        match expr with
        | Patterns.LetVar (var, letExpr, inExpr) ->
            if isPrimitiveExpression letExpr then
                Expr.Let(var, letExpr, varDefsToLambda inExpr)
            else
                let fType = FSharpType.MakeFunctionType(typeof<unit>, var.Type)
                let fVar = Var(var.Name + "UnitFunc", fType)

                let letExprNew =
                    Expr.Let(
                        fVar,
                        Expr.Lambda(Var("unitVar", typeof<unit>), varDefsToLambda letExpr),
                        Expr.Application(Expr.Var(fVar), Expr.Value((), typeof<unit>))
                    )

                Expr.Let(var, letExprNew, varDefsToLambda inExpr)

        | ExprShape.ShapeVar _ -> expr
        | ExprShape.ShapeLambda (var, body) -> Expr.Lambda(var, varDefsToLambda body)
        | ExprShape.ShapeCombination (shapeComboObject, exprList) ->
            let exprList' = List.map varDefsToLambda exprList
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList')
