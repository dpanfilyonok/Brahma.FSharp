namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator
open System
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

    let private modifyFirstOfList f lst =
        match lst with
        | x :: tail -> f x :: tail
        | _ -> invalidArg "lst" "List should not be empty"

    let private modifyFirstOfListList f lst =
        match lst with
        | [x] :: tail -> [f x] :: tail
        | _ -> invalidArg "lst" "List should not be empty"

    let private getFirstOfListListWith f lst =
        match lst with
        | [x] :: _ -> f x
        | _ -> invalidArg "lst" "List should not be empty"

    let rec private transformAtomicsAndCollectPointerVars (expr: Expr) = state {
        match expr with
        | DerivedPatterns.Applications
            (
                DerivedPatterns.SpecificCall <@ atomic @>
                    (
                        _,
                        _,
                        [DerivedPatterns.Lambdas (lambdaArgs, lambdaBody)]
                    ),
                ([Patterns.ValidVolatileArg pointerVar] :: _ as applicationArgs)
            ) ->

            let newApplicationArgs =
                applicationArgs
                |> List.collect id
                |> modifyFirstOfList Utils.createRefCall

            // https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/atomicFunctions.html
            match lambdaBody with
            | DerivedPatterns.SpecificCall <@ (+) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAdd @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (-) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicSub @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, [Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicInc @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, [Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicDec @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ xchg @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> || onType = typeof<float32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXchg @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, [Patterns.Var _; Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicCmpxchg @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ min @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMin @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ max @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMax @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (&&&) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAnd @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (|||) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicOr @>, newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (^^^) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXor @>, newApplicationArgs)

            | _ ->
                let collectedLambdaTypes =
                    lambdaArgs
                    |> List.collect id
                    |> List.map (fun var -> var.Type)
                    |> fun args -> args @ [lambdaBody.Type]

                (* baseFunc *)

                let baseFuncType =
                    collectedLambdaTypes
                    |> Utils.makeLambdaType

                let baseFuncVar = Var("baseFunc", baseFuncType)

                let baseFuncArgs = lambdaArgs

                let baseFuncBody =
                    match lambdaBody with
                    | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, _) ->
                        failwithf "Atomic inc for %O is not suppotred" onType

                    | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, _) ->
                        failwithf "Atomic inc for %O is not suppotred" onType

                    | DerivedPatterns.SpecificCall <@ xchg @> (_, _, [Patterns.Var p; Patterns.Var value]) ->
                        Expr.Var value

                    | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, _, [Patterns.Var p; Patterns.Var cmp; Patterns.Var value]) ->
                        Expr.IfThenElse(
                            Expr.Call(Utils.getMethodInfoOfLambda <@ (=) @>, [Expr.Var p; Expr.Var cmp]),
                            Expr.Var value,
                            Expr.Var p
                        )

                    | _ -> lambdaBody

                (* atomicFunc*)

                let atomicFuncType =
                    collectedLambdaTypes
                    |> modifyFirstOfList (fun x -> typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x))
                    |> Utils.makeLambdaType

                let atomicFuncVar = Var("atomicFunc", atomicFuncType)

                let atomicFuncArgs =
                    baseFuncArgs
                    |> modifyFirstOfListList
                        (fun x ->
                            Var(
                                x.Name,
                                typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x.Type),
                                x.IsMutable
                            )
                        )

                let mutexVar =
                    Var(
                        pointerVar.Name + "Mutex",
                        if pointerVar.Type.IsArray then
                            typeof<Mutex[]>
                        else
                            typeof<Mutex>
                    )

                do! State.modify (fun (state: Map<Var, Var>) -> state |> Map.add pointerVar mutexVar)

                let atomicFuncBody =
                    let mutex =
                        applicationArgs
                        |> getFirstOfListListWith
                            (fun expr ->
                                match expr with
                                | Patterns.Var _ -> Expr.Var mutexVar
                                | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var _; idx]) ->
                                    Expr.Call(
                                        Utils.getMethodInfoOfLambda <@ IntrinsicFunctions.GetArray<Mutex> @>,
                                        [Expr.Var mutexVar; idx]
                                    )
                                | _ -> failwith "Invalid volatile argument. This exception should never occur :)"
                            )
                        |> Utils.createRefCall

                    let baseFuncApplicaionArgs =
                        atomicFuncArgs
                        |> List.map (List.map Expr.Var)
                        |> modifyFirstOfListList Utils.createDereferenceCall

                    let oldValueVar =
                        Var(
                            "oldValue",
                            getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicFuncArgs,
                            true
                        )

                    Expr.Let(
                        oldValueVar,
                        Expr.DefaultValue <| getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicFuncArgs,
                        Expr.Sequential(
                            <@@
                                let mutable flag = true
                                while flag do
                                    let old = atomicXchg %%mutex 1
                                    if old = 0 then
                                        %%Expr.VarSet(
                                            oldValueVar,
                                            getFirstOfListListWith id baseFuncApplicaionArgs
                                        )
                                        %%(
                                            Utils.createReferenceSetCall
                                            <| getFirstOfListListWith Expr.Var atomicFuncArgs
                                            <| Expr.Applications(Expr.Var baseFuncVar, baseFuncApplicaionArgs)
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
                        Expr.Lambdas(baseFuncArgs, baseFuncBody),
                        Expr.Let(
                            atomicFuncVar,
                            Expr.Lambdas(atomicFuncArgs, atomicFuncBody),
                            Expr.Applications(
                                Expr.Var atomicFuncVar,
                                newApplicationArgs |> List.map List.singleton
                            )
                        )
                    )

        | DerivedPatterns.Applications
            (
                DerivedPatterns.SpecificCall <@ atomic @>
                    (
                        _,
                        _,
                        [DerivedPatterns.Lambdas _]
                    ),
                [invalidVolatileArg] :: _
            ) ->
            return failwithf
                "Invalid volatile arg of atomic function. Must be `var` of `var.[expr]`, \
                where `var` is variable in local or global memory, but given\n%O" invalidVolatileArg

        | ExprShape.ShapeVar var -> return Expr.Var var
        | ExprShape.ShapeLambda (var, lambda) ->
            let! transformedLambda = transformAtomicsAndCollectPointerVars lambda
            return Expr.Lambda(var, transformedLambda)
        | ExprShape.ShapeCombination (combo, exprs) ->
            let! transformedList = exprs |> List.map transformAtomicsAndCollectPointerVars |> State.collect
            return ExprShape.RebuildShapeCombination(combo, transformedList)
    }

    let private insertMutexVars (expr: Expr) = state {
        let! (pointerVarToMutexVarMap: Map<Var, Var>) = State.get
        match expr with
        | DerivedPatterns.Lambdas (args, body) ->
            let args = List.collect id args
            let newArgs = ResizeArray args

            // Set global args
            pointerVarToMutexVarMap
            |> Map.iter
                (fun var mutexVar ->
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
                        match pointerVarToMutexVarMap |> Map.tryFind var with
                        | Some mutexVar -> Expr.Let(mutexVar, letExpr, inExpr)
                        | None -> inExpr
                    )

                | ExprShape.ShapeVar var -> Expr.Var var
                | ExprShape.ShapeLambda (var, lambda) ->
                    Expr.Lambda(var, go lambda)
                | ExprShape.ShapeCombination (combo, exprs) ->
                    ExprShape.RebuildShapeCombination(combo, List.map go exprs)

            return Expr.Lambdas(Seq.toList newArgs |> List.map List.singleton, go body)

        | _ -> return raise <| InvalidKernelException(sprintf "Invalid kernel expression. Must be lambda, but given\n%O" expr)
    }

    let processAtomic (expr: Expr) =
        transformAtomicsAndCollectPointerVars expr
        >>= insertMutexVars
        |> State.eval Map.empty
