namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open FSharp.Reflection
open System.Reflection
open Brahma.FSharp.OpenCL.Translator
open System
open Brahma.FSharp.OpenCL.Extensions
open FSharp.Core.LanguagePrimitives

type Mutex = int

[<AutoOpen>]
module AtomicProcessor =
    let inline private atomicAdd p v = (+) !p v
    let inline private atomicSub p v = (-) !p v
    let inline private atomicInc p = inc !p
    let inline private atomicDec p = dec !p
    let inline private atomicXchg p v = xchg !p v
    let inline private atomicCmpxchg p cmp v = cmpxchg !p cmp v
    let inline private atomicMin p v = min !p v
    let inline private atomicMax p v = max !p v
    let inline private atomicAnd p v = (&&&) !p v
    let inline private atomicOr p v = (|||) !p v
    let inline private atomicXor p v = (^^^) !p v
    // let private atomicF (mutex: Mutex ref) f = atomic f

    let private modifyFirstOfList f lst =
        match lst with
        | x :: tail -> f x :: tail
        | _ -> raise <| ArgumentException "List should not be empty"

    let private modifyFirstOfListList f lst =
        match lst with
        | [x] :: tail -> [f x] :: tail
        | _ -> raise <| ArgumentException "List should not be empty"

    let private getFirstOfListListWith f lst =
        match lst with
        | [x] :: _ -> f x
        | _ -> raise <| ArgumentException "List should not be empty"

    /// Extract ```var``` from expression if expression is ```var``` or ```var.[idx]```
    let private extractVar (expr: Expr) =
        match expr with
        | Patterns.Var var -> var
        | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var arrayVar; idx]) -> arrayVar
        | _ -> raise <| ArgumentException "Expression should be either 'var' or 'var.[expr]'"

    /// <summary>
    /// <code>
    /// atomic (fun x y -> x + y) a b
    /// -> atomicAdd a b
    /// -> let f = (fun x y -> x + y) in
    ///    let atomicFunc x y = atomicF aMutex f x y in
    ///    atomicFunc a b
    /// </code>
    /// </summary>
    let rec private transformAtomicsAndCollectVars (expr: Expr) = state {
        match expr with
        // atomic application can't be inside lambda
        // let a b = atomic (fun x y -> x + y) a b is not supported
        | DerivedPatterns.Lambdas
            (
                _,
                DerivedPatterns.Applications
                    (
                        DerivedPatterns.SpecificCall <@ atomic @> (_, _, _),
                        applicationArgs
                    )
            ) ->
            return failwith "lol"

        | DerivedPatterns.Applications
            (
                DerivedPatterns.SpecificCall <@ atomic @>
                    (
                        _,
                        _,
                        [DerivedPatterns.Lambdas (lambdaArgs, lambdaBody)]
                    ),
                applicationArgs
            ) ->
            // нативные атомики будут генерироваться, если функция без аргументов
            // TODO можно сделать так, чтобы и для частично примененных функций так работало
            // atomic (fun x -> x + 1) buffer.[0] => atomicAdd buffer.[0] 1
            match lambdaBody with
            | DerivedPatterns.SpecificCall <@ (+) @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAdd @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (-) @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicSub @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicInc @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicDec @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ xchg @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> || onType = typeof<float32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXchg @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicCmpxchg @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ min @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMin @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ max @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMax @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (&&&) @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAnd @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (|||) @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicOr @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (^^^) @> (_, onType :: _, []) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXor @>, List.collect id applicationArgs)

            | _ ->
                let collectedLambdaTypes =
                    lambdaArgs
                    |> List.collect id
                    |> List.map (fun var -> var.Type)
                    |> fun args -> args @ [lambdaBody.Type]

                let pointerVar = getFirstOfListListWith extractVar applicationArgs
                let mutexVar =
                    Var(
                        pointerVar.Name + "Mutex",
                        if pointerVar.Type.IsArray then
                            typeof<Mutex[]>
                        else
                            typeof<Mutex>
                    )

                do! State.modify (fun (state: Map<Var, Var>) -> state |> Map.add pointerVar mutexVar)

                let baseFuncType =
                    collectedLambdaTypes
                    |> Utils.makeLambdaType

                let atomicFuncType =
                    collectedLambdaTypes
                    |> modifyFirstOfList (fun x -> typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x))
                    |> Utils.makeLambdaType

                let baseFuncVar = Var("baseFunc", baseFuncType)
                let atomicFuncVar = Var("atomicFunc", atomicFuncType)

                let atomicArgs =
                    lambdaArgs
                    |> modifyFirstOfListList
                        (fun x ->
                            Var(
                                x.Name,
                                typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x.Type),
                                x.IsMutable
                            )
                        )

                let atomicBody =
                    let mutex =
                        applicationArgs
                        |> getFirstOfListListWith
                            (fun expr ->
                                expr.Substitute(fun var ->
                                    if var.Name = pointerVar.Name then
                                        Some (Expr.Var mutexVar)
                                    else
                                        None
                                )
                            )
                        |> Utils.createRefCall

                    let atomicApplicaionArgs =
                        atomicArgs
                        |> List.map (List.map Expr.Var)
                        |> modifyFirstOfListList Utils.createDereferenceCall

                    let oldValueVar =
                        Var(
                            "oldValue",
                            getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicArgs,
                            true
                        )

                    Expr.Let(
                        oldValueVar,
                        Expr.DefaultValue <| getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicArgs,
                        Expr.Sequential(
                            <@@
                                let mutable flag = true
                                while flag do
                                    let old = atomicXchg %%mutex 1
                                    if old = 0 then
                                        %%Expr.VarSet(
                                            oldValueVar,
                                            getFirstOfListListWith id atomicApplicaionArgs
                                        )
                                        %%(
                                            Utils.createReferenceSetCall
                                            <| getFirstOfListListWith Expr.Var atomicArgs
                                            <| Expr.Applications(Expr.Var baseFuncVar, atomicApplicaionArgs)
                                        )
                                        atomicXchg %%mutex 0 |> ignore
                                        flag <- false
                                barrier ()
                            @@>,
                            Expr.Var oldValueVar
                        )
                    )

                return
                    Expr.Let(
                        baseFuncVar,
                        Expr.Lambdas(lambdaArgs, lambdaBody),
                        Expr.Let(
                            atomicFuncVar,
                            Expr.Lambdas(atomicArgs, atomicBody),
                            Expr.Applications(
                                Expr.Var atomicFuncVar,
                                applicationArgs |> modifyFirstOfListList Utils.createRefCall
                            )
                        )
                    )

        | ExprShape.ShapeVar var -> return Expr.Var var
        | ExprShape.ShapeLambda (var, lambda) ->
            let! transformedLambda = transformAtomicsAndCollectVars lambda
            return Expr.Lambda(var, transformedLambda)
        | ExprShape.ShapeCombination (combo, exprs) ->
            let! transformedList = exprs |> List.map transformAtomicsAndCollectVars |> State.collect
            return ExprShape.RebuildShapeCombination(combo, transformedList)
    }

    let private insertMutexVars (expr: Expr) = state {
        let! (varToMutexVarMap: Map<Var, Var>) = State.get
        match expr with
        | DerivedPatterns.Lambdas (args, body) ->
            let args = List.collect id args
            let newArgs = ResizeArray args

            // Set global args
            // TODO хорошо бы не в конец добавлять, а в пару к той переменной для которой этот мьютекс предназначен
            varToMutexVarMap
            |> Map.iter (fun var mutexVar ->
                if List.contains var args then
                    newArgs.Add mutexVar
            )

            // Set local args
            let rec go expr =
                match expr with
                | Patterns.Let (var, (DerivedPatterns.SpecificCall <@ local @> (_, _, _) as letExpr), inExpr)
                | Patterns.Let (var, (DerivedPatterns.SpecificCall <@ localArray @> (_, _, _) as letExpr), inExpr) ->
                    Expr.Let(
                        var,
                        letExpr,
                        match varToMutexVarMap |> Map.tryFind var with
                        | Some mutexVar -> Expr.Let(mutexVar, letExpr, inExpr)
                        | None -> inExpr
                    )

                | ExprShape.ShapeVar var -> Expr.Var var
                | ExprShape.ShapeLambda (var, lambda) ->
                    Expr.Lambda(var, go lambda)
                | ExprShape.ShapeCombination (combo, exprs) ->
                    ExprShape.RebuildShapeCombination(combo, List.map go exprs)

            return Expr.Lambdas(Seq.toList newArgs |> List.map List.singleton, go body)

        | _ -> return failwith "dsds"
    }

    let processAtomic (expr: Expr) =
        transformAtomicsAndCollectVars expr
        >>= insertMutexVars
        |> State.eval Map.empty

// TODO неплохо было бы контекст и на qt распространить, тк много можно во время трансформации получать
