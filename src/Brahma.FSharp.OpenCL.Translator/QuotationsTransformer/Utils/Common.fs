module Brahma.FSharp.OpenCL.QuotationsTransformer.Utils.Common

open FSharp.Reflection
open FSharp.Quotations

let rec getFunctionArgTypes (funType: System.Type) =
    let (argType, retType) = FSharpType.GetFunctionElements(funType)
    match retType with
    | _ when FSharpType.IsFunction retType ->
        argType :: getFunctionArgTypes retType
    | _ ->  [argType]

let isFunction (var: Var) =
    FSharpType.IsFunction var.Type

/// Collect free variables of expression, that satisfies predicate.
let rec collectFreeVarsWithPredicate (predicate: Var -> bool) (expr: Expr): Set<Var> =
    match expr with
    | Patterns.Let (var, expr, inExpr) ->
        Set.union <|
            collectFreeVarsWithPredicate predicate expr <|
            Set.remove var (collectFreeVarsWithPredicate predicate inExpr)

    | ExprShape.ShapeVar var ->
        if predicate var then Set.singleton var else Set.empty

    | ExprShape.ShapeLambda (var, expr) ->
        expr
        |> collectFreeVarsWithPredicate predicate
        |> Set.remove var

    | ExprShape.ShapeCombination (_, exprs) ->
        exprs
        |> List.map (collectFreeVarsWithPredicate predicate)
        |> Set.unionMany

let collectFreeVars: Expr -> Set<Var> =
    collectFreeVarsWithPredicate (not << isFunction)

let collectFreeFunctionVars: Expr -> Set<Var> =
    collectFreeVarsWithPredicate isFunction
