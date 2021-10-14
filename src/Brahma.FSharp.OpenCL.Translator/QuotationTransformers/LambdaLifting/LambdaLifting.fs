namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

[<AutoOpen>]
module LambdaLifting =
    let rec parameterLiftExprImpl (ctx: Context) (expr: Expr) =
        match expr with
        | Patterns.LetVar (v, definition, inExpr) ->
            Expr.Let(
                v,
                parameterLiftExprImpl ctx definition,
                parameterLiftExprImpl ctx inExpr
            )

        | Patterns.LetFunc (f, definition, inExpr) ->
            let localFreeVars = Utils.collectFreeVars definition
            let freeFunctionVars = Utils.collectFreeFunctionVars definition

            let getSetFreeVars (fVar: Var) =
                Context.getFunctionFreeVariables fVar ctx
                |> Option.defaultValue List.empty
                |> Set.ofList

            let extendedFreeVars =
                freeFunctionVars
                |> Set.map getSetFreeVars
                |> Set.unionMany

            let freeVars = Set.union localFreeVars extendedFreeVars |> Set.toList

            let (substitution, newFuncVar) = Context.makeApplication f freeVars
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

        // | Patterns.PropertySet (o, prop, idxs, value) ->
        //     // let localFreeVars = Utils.collectFreeVars definition
        //     // let freeFunctionVars = Utils.collectFreeFunctionVars definition

        //     // let getSetFreeVars (fVar: Var) =
        //     //     Context.getFunctionFreeVariables fVar ctx
        //     //     |> Option.defaultValue List.empty
        //     //     |> Set.ofList

        //     // let extendedFreeVars =
        //     //     freeFunctionVars
        //     //     |> Set.map getSetFreeVars
        //     //     |> Set.unionMany

        //     // let freeVars = Set.union localFreeVars extendedFreeVars |> Set.toList

        //     // let (substitution, newFuncVar) = Context.makeApplication f freeVars
        //     // let newDefinition = parameterLiftExprImpl ctx definition
        //     // let extendedCtx =
        //     //     ctx
        //     //     |> Context.setFunctionFreeVariables f freeVars
        //     //     |> Context.setFunctionSubstitution f substitution

        //     failwith ""

        | ExprShape.ShapeLambda (x, body) ->
            Expr.Lambda(x, parameterLiftExprImpl ctx body)

        | ExprShape.ShapeVar var ->
            match Context.getFunctionSubstitution var ctx with
            | Some subst -> subst
            | None -> expr

        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            ExprShape.RebuildShapeCombination(shapeComboObject, List.map (parameterLiftExprImpl ctx) exprList)

    let parameterLiftExpr =
        parameterLiftExprImpl Context.empty

    let rec blockFloating (expr: Expr) =
        match expr with
        | Patterns.LetFunc(var, body, inExpr) ->
            let (body', bodyMethods) = blockFloating body
            let (inExpr', inExprMethods) = blockFloating inExpr
            inExpr', bodyMethods @ [ (var, body') ] @ inExprMethods

        | ExprShape.ShapeLambda (var, body) ->
            let (body', methods) = blockFloating body
            Expr.Lambda(var, body'), methods

        | ExprShape.ShapeVar var ->
            Expr.Var(var), List.empty

        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            let (exprList', methods) = exprList |> List.map blockFloating |> List.unzip
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList'), List.concat methods

    let lambdaLifting (expr: Expr) =
        expr
        |> parameterLiftExpr
        |> VoidArgumentsCleanUp.cleanUpVoidArguments
        |> blockFloating
