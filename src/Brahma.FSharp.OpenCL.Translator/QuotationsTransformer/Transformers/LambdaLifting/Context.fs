namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations

type Context =
    private {
        FreeVariables: Map<Var, List<Var>>
        Substitution: Map<Var, Expr>
    }

module Context =
    /// head: t, args: [x1: t1; x2: t2; x3: t3]
    /// result: (newHead: t1 -> t2 -> t3 -> t) x1 x2 x3
    let makeApplication (head: Var) (args: List<Var>) =
        let argTypes = List.map (fun (x: Var) -> x.Type) args
        let newHeadType = Utils.makeFunctionType head.Type argTypes

        let newHead = Var(head.Name, newHeadType, head.IsMutable)
        let application =
            args
            |> List.map Expr.Var
            |> List.fold (fun acc expr -> Expr.Application(acc, expr)) (Expr.Var newHead)

        application, newHead

    let empty = { FreeVariables = Map.empty; Substitution = Map.empty }

    let setFunctionFreeVariables (oldFuncVar: Var) (extendedParams: List<Var>) (ctx: Context) =
        {
            FreeVariables = ctx.FreeVariables.Add(oldFuncVar, extendedParams)
            Substitution = ctx.Substitution
        }

    let setFunctionSubstitution (oldFuncVar: Var) (substitution: Expr) (ctx: Context) =
        {
            FreeVariables = ctx.FreeVariables
            Substitution = ctx.Substitution.Add(oldFuncVar, substitution)
        }

    let getFunctionFreeVariables (oldFuncVar: Var) (ctx: Context) =
        ctx.FreeVariables.TryFind oldFuncVar

    let getFunctionSubstitution (oldFuncVar: Var) (ctx: Context) =
        ctx.Substitution.TryFind oldFuncVar
