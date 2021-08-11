namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator
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

    let private atomicAddInfo = (Utils.getMethodInfoOfLambda <@ atomicAdd @>).GetGenericMethodDefinition()
    let private atomicSubInfo = (Utils.getMethodInfoOfLambda <@ atomicSub @>).GetGenericMethodDefinition()
    let private atomicIncInfo = (Utils.getMethodInfoOfLambda <@ atomicInc @>).GetGenericMethodDefinition()
    let private atomicDecInfo = (Utils.getMethodInfoOfLambda <@ atomicDec @>).GetGenericMethodDefinition()
    let private atomicXchgInfo = (Utils.getMethodInfoOfLambda <@ atomicXchg @>).GetGenericMethodDefinition()
    let private atomicCmpxchgInfo = (Utils.getMethodInfoOfLambda <@ atomicCmpxchg @>).GetGenericMethodDefinition()
    let private atomicMinInfo = (Utils.getMethodInfoOfLambda <@ atomicMin @>).GetGenericMethodDefinition()
    let private atomicMaxInfo = (Utils.getMethodInfoOfLambda <@ atomicMax @>).GetGenericMethodDefinition()
    let private atomicAndInfo = (Utils.getMethodInfoOfLambda <@ atomicAnd @>).GetGenericMethodDefinition()
    let private atomicOrInfo = (Utils.getMethodInfoOfLambda <@ atomicOr @>).GetGenericMethodDefinition()
    let private atomicXorInfo = (Utils.getMethodInfoOfLambda <@ atomicXor @>).GetGenericMethodDefinition()

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
                ([Patterns.ValidVolatileArg pointerVar as volatileArg] :: _ as applicationArgs)
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
                return Expr.Call(atomicAddInfo.MakeGenericMethod(onType, onType, onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (-) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicSubInfo.MakeGenericMethod(onType, onType, onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, [Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicIncInfo.MakeGenericMethod(onType, onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, [Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicDecInfo.MakeGenericMethod(onType, onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ xchg @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> || onType = typeof<float32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicXchgInfo.MakeGenericMethod(onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, [Patterns.Var _; Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicCmpxchgInfo.MakeGenericMethod(onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ min @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // TODO если устройство не поддерживает атомики для этих типов, то вообще работать не будет
                // нужно либо забить на расширения, либо учитывать параметры девайса
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicMinInfo.MakeGenericMethod(onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ max @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicMaxInfo.MakeGenericMethod(onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (&&&) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicAndInfo.MakeGenericMethod(onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (|||) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicOrInfo.MakeGenericMethod(onType), newApplicationArgs)

            | DerivedPatterns.SpecificCall <@ (^^^) @> (_, onType :: _, [Patterns.Var _; Patterns.Var _]) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                return Expr.Call(atomicXorInfo.MakeGenericMethod(onType), newApplicationArgs)

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

                    | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, [Patterns.Var p; Patterns.Var cmp; Patterns.Var value]) ->
                        Expr.IfThenElse(
                            Expr.Call((Utils.getMethodInfoOfLambda <@ (=) @>).GetGenericMethodDefinition().MakeGenericMethod(onType), [Expr.Var p; Expr.Var cmp]),
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

                let! (state: Map<Var, Var>) = State.get

                let mutexVar =
                    match state |> Map.tryFind pointerVar with
                    | Some x -> x
                    | None ->
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
                        match volatileArg with
                        | Patterns.Var _ -> Expr.Var mutexVar
                        | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var _; idx]) ->
                            Expr.Call(
                                Utils.getMethodInfoOfLambda <@ IntrinsicFunctions.GetArray<Mutex> @>,
                                [Expr.Var mutexVar; idx]
                            )
                        | _ -> failwith "Invalid volatile argument. This exception should never occur :)"
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
            |> Map.iter (fun var mutexVar ->
                if args |> List.contains var then
                    newArgs.Add mutexVar
            )

            // Set local args
            let rec go expr =
                match expr with
                | Patterns.Let (var, (DerivedPatterns.SpecificCall <@ local @> (_, _, args) as letExpr), inExpr) ->
                    Expr.Let(
                        var,
                        letExpr,
                        match pointerVarToMutexVarMap |> Map.tryFind var with
                        | Some mutexVar ->
                            Expr.Let(
                                mutexVar,
                                Expr.Call(Utils.getMethodInfoOfLambda <@ local<int> @>, args),
                                inExpr
                            )
                        | None -> inExpr
                    )
                | Patterns.Let (var, (DerivedPatterns.SpecificCall <@ localArray @> (_, _, args) as letExpr), inExpr) ->
                    Expr.Let(
                        var,
                        letExpr,
                        match pointerVarToMutexVarMap |> Map.tryFind var with
                        | Some mutexVar ->
                            Expr.Let(
                                mutexVar,
                                Expr.Call(Utils.getMethodInfoOfLambda <@ localArray<int> @>, args),
                                inExpr
                            )
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
