module Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LambdaLifting.ParameterLifting

open FSharp.Quotations
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LambdaLifting.Context
open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils.Common
open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils.Patterns

let rec parameterLiftExprImpl (ctx: Context) (expr: Expr): Expr =
    match expr with
    | LetVar(v, definition, inExpr) ->
        Expr.Let(
            v,
            parameterLiftExprImpl ctx definition,
            parameterLiftExprImpl ctx inExpr
        )

    | LetFunc(f, definition, inExpr) ->
        let localFreeVars = collectFreeVars definition

        let freeFunctionVars = collectFreeFunctionVars definition

        let getSetFreeVars (fVar: Var) =
            Context.getFunctionFreeVariables fVar ctx
            |> Option.defaultValue List.empty
            |> Set.ofList

        let extendedFreeVars =
            freeFunctionVars
            |> Set.map getSetFreeVars
            |> Set.unionMany

        let freeVars = Set.union localFreeVars extendedFreeVars |> Set.toList

        let substitution, newFuncVar = makeApplication f freeVars
        let newDefinition = parameterLiftExprImpl ctx definition
        let extendedCtx =
            ctx
            |> Context.setFunctionFreeVariables f freeVars
            |> Context.setFunctionSubstitution f substitution

        Expr.Let(
            newFuncVar,
            List.foldBack (fun arg body -> Expr.Lambda(arg, body)) freeVars newDefinition,
            inExpr |> parameterLiftExprImpl extendedCtx
        )

    | ExprShape.ShapeLambda (x, body) ->
        Expr.Lambda(x, parameterLiftExprImpl ctx body)

    | ExprShape.ShapeVar var ->
        match Context.getFunctionSubstitution var ctx with
        | Some subst ->
            subst
        | None -> expr

    | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
        ExprShape.RebuildShapeCombination(shapeComboObject, List.map (parameterLiftExprImpl ctx) exprList)

let parameterLiftExpr: Expr -> Expr =
    parameterLiftExprImpl Context.empty
