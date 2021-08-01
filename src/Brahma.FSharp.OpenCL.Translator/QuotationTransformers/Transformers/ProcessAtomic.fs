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
module ProcessAtomic =
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
    let private atomicF (mutex: Mutex ref) f = atomic f

    let private modifyFirstOfList f lst =
        match lst with
        | x :: tail -> f x :: tail
        | _ -> failwithf "Empty list"

    let private modifyFirstOfListList f lst =
        match lst with
        | [x] :: tail -> [f x] :: tail
        | _ -> failwithf "meh"

    let private getFirstOfListListWith f lst =
        match lst with
        | [x] :: _ -> f x
        | _ -> failwith "aaa"

    let private extractVarFromAtomicAppl (expr: Expr) =
        match expr with
        | Patterns.Var var -> var
        | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var arrayVar; idx]) -> arrayVar
        | _ -> failwith "F"

    let private createMutexFromAtomicAppl (expr: Expr) oldName (vM: Var) =
        expr.Substitute(fun v -> if v.Name = oldName then Some (Expr.Var vM) else None)

    /// <summary>
    /// <code>
    /// atomic (fun x y -> x + y) a b
    /// -> atomicAdd a b
    /// -> let f = (fun x y -> x + y) in
    ///    let atomicFunc x y = atomicF aMutex f x y in
    ///    atomicFunc a b
    /// </code>
    /// </summary>
    let rec transformAtomicsAndCollectVars (expr: Expr) = state {
        match expr with
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

            match lambdaBody with
            | DerivedPatterns.SpecificCall <@ (+) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAdd @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (-) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicSub @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicInc @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicDec @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ xchg @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> || onType = typeof<float32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXchg @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicCmpxchg @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ min @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMin @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ max @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMax @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (&&&) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAnd @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (|||) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicOr @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (^^^) @> (_, onType :: _, _) when
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

                let v = getFirstOfListListWith extractVarFromAtomicAppl applicationArgs
                let vMutex =
                    Var(
                        v.Name + "Mutex",
                        if v.Type.IsArray then
                            typeof<Mutex[]>
                        else
                            typeof<Mutex>
                    )

                do! State.modify (fun (state: Map<Var, Var>) -> state |> Map.add v vMutex)

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
                        getFirstOfListListWith (fun x -> createMutexFromAtomicAppl x v.Name vMutex) applicationArgs
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
                                    // NOTE тут если оставлять без привязки, то полсе этого выражения вставляется unit, а его транслировать не умеем
                                    atomicXchg %%mutex 0 |> ignore
                                    flag <- false
                            barrier ()
                            // TODO тут он как то криво тип выозвращаемого значения выводит (не выводит сосвсем)
                            (%%Expr.Var oldValueVar : int)
                        @@>
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

    let insertMutexVars (expr: Expr) = state {
        let! (st : Map<Var, Var>) = State.get
        let (args, body) = Utils.extractLambdaArguments expr
        let ra = ResizeArray args
        st
        |> Map.toList
        |> List.iter
            (fun (var, mutexVar) ->
                if List.contains var args then
                    ra.Add mutexVar
            )

        let rec loop expr =
            match expr with
            | Patterns.Let (variable, (DerivedPatterns.SpecificCall <@ local @> (_, _, _) as body), cont) ->
                Expr.Let(
                    variable,
                    body,
                    match st |> Map.tryFind variable with
                    | Some vMutex ->
                        Expr.Let(
                            vMutex,
                            body,
                            cont
                        )
                    | None ->
                        cont
                )
            | Patterns.Let (variable, (DerivedPatterns.SpecificCall <@ localArray @> (_, _, _) as body), cont) ->
                Expr.Let(
                    variable,
                    body,
                    match st |> Map.tryFind variable with
                    | Some vMutex ->
                        Expr.Let(
                            vMutex,
                            body,
                            cont
                        )
                    | None ->
                        cont
                )

            | ExprShape.ShapeVar var -> Expr.Var var
            | ExprShape.ShapeLambda (var, lambda) ->
                Expr.Lambda(var, loop lambda)
            | ExprShape.ShapeCombination (combo, exprs) ->
                ExprShape.RebuildShapeCombination(combo, List.map loop exprs)

        return Expr.Lambdas(Seq.toList ra |> List.map List.singleton, loop body)
    }

    let processAtomic (expr: Expr) =
        transformAtomicsAndCollectVars expr
        >>= insertMutexVars
        |> State.eval Map.empty
