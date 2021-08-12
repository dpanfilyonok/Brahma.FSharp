module Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LambdaLifting.Context

open FSharp.Quotations
open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils

/// head: t, args: [x1: t1; x2: t2; x3: t3]
/// result: (newHead: t1 -> t2 -> t3 -> t) x1 x2 x3
let makeApplication (head: Var) (args: List<Var>) =
    let argTypes = List.map (fun (x: Var) -> x.Type) args
    let newHeadType = Common.makeFunctionType head.Type argTypes

    let newHead = Var(head.Name, newHeadType, head.IsMutable)
    let application =
        args
        |> List.map Expr.Var
        |> List.fold (fun acc expr -> Expr.Application(acc, expr)) (Expr.Var newHead)
    application, newHead

type Context =
    private
        { freeVariables: Map<Var, List<Var>>
          substitution: Map<Var, Expr> }

module Context =
    let empty = { freeVariables = Map.empty; substitution = Map.empty }

    let setFunctionFreeVariables (oldFuncVar: Var) (extendedParams: List<Var>) (ctx: Context) =
        {
            freeVariables = ctx.freeVariables.Add(oldFuncVar, extendedParams)
            substitution = ctx.substitution
        }

    let setFunctionSubstitution (oldFuncVar: Var) (substitution: Expr) (ctx: Context) =
        {
            freeVariables = ctx.freeVariables
            substitution = ctx.substitution.Add(oldFuncVar, substitution)
        }

    let getFunctionFreeVariables (oldFuncVar: Var) (ctx: Context) =
        ctx.freeVariables.TryFind oldFuncVar

    let getFunctionSubstitution (oldFuncVar: Var) (ctx: Context) =
        ctx.substitution.TryFind oldFuncVar
