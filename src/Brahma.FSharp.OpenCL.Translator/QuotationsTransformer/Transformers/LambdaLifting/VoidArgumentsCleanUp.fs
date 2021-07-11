namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations
open FSharp.Reflection

module VoidArgumentsCleanUp =
    let private isConsistOfVoidVarOnly (args: list<Var>) =
        args.Length = 1 && args.Head.Type = typeof<unit>

    let private isConsistOfVoidExprOnly (args: list<Expr>) =
        args.Length = 1 && args.Head.Type = typeof<unit>

    let rec private cleanUpVoidArgumentsImpl (subst: Map<Var, Var>) (expr: Expr) =
        match expr with
        | Patterns.LetFuncUncurry (var, args, body, inExpr) ->
            let args' =
                if isConsistOfVoidVarOnly args then args
                else List.filter (not << Utils.isTypeOf<unit>) args

            let newFuncVarType = Utils.makeFunctionType body.Type <| List.map (fun (var: Var) -> var.Type) args'
            let newFuncVar = Var(var.Name, newFuncVarType, var.IsMutable)
            let body' = cleanUpVoidArgumentsImpl subst body

            let subst' = subst.Add (var, newFuncVar)
            let inExpr' = cleanUpVoidArgumentsImpl subst' inExpr
            Expr.Let(newFuncVar, Utils.makeLambdaExpr args' body', inExpr')

        | Patterns.ApplicationUncurry (head, exprs) ->
            match head with
            | Patterns.Var var ->
                match subst.TryFind var with
                | Some var' ->
                    let exprs' =
                        if isConsistOfVoidExprOnly exprs then exprs
                        else List.filter (fun (exp: Expr) -> exp.Type <> typeof<unit>) exprs

                    Utils.makeApplicationExpr
                    <| Expr.Var var'
                    <| List.map (cleanUpVoidArgumentsImpl subst) exprs'

                | _ -> expr
            | _ -> expr
        | ExprShape.ShapeLambda (var, body) ->
            Expr.Lambda (var, cleanUpVoidArgumentsImpl subst body)
        | ExprShape.ShapeVar var ->
            match subst.TryFind var with
            | Some _ ->
                // TODO: check it in another step.
                failwithf "First-Order functions (just like curring) is not supported."
            | None -> expr
        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            let exprList' = List.map <| cleanUpVoidArgumentsImpl subst <| exprList
            ExprShape.RebuildShapeCombination (shapeComboObject, exprList')

    let cleanUpVoidArguments (expr: Expr) =
        cleanUpVoidArgumentsImpl Map.empty expr
