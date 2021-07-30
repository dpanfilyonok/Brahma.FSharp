namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

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

    // let processP (expr: Expr) =
    //     match expr with
    //     | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var array; idx]) ->
    //         expr.Substitute <| function v | v when v.Name =

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

    /// <summary>
    /// <code>
    /// atomic (fun x y -> x + y) a b
    /// -> atomicAdd a b
    /// -> let f = (fun x y -> x + y) in
    ///    let atomicFunc x y = atomicF aMutex f x y in
    ///    atomicFunc a b
    /// </code>
    /// </summary>
    let rec processAtomic (expr: Expr) =
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
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAdd @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (-) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicSub @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicInc @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicDec @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ xchg @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> || onType = typeof<float32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXchg @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // base
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicCmpxchg @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ min @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMin @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ max @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicMax @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (&&&) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicAnd @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (|||) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicOr @>, List.collect id applicationArgs)

            | DerivedPatterns.SpecificCall <@ (^^^) @> (_, onType :: _, _) when
                onType = typeof<int> || onType = typeof<uint32> ||
                // extended
                onType = typeof<int64> || onType = typeof<uint64> ->
                Expr.Call(Utils.getMethodInfoOfLambda <@ atomicXor @>, List.collect id applicationArgs)

            | _ ->
                let collectedLambdaTypes =
                    lambdaArgs
                    |> List.collect id
                    |> List.map (fun var -> var.Type)
                    |> fun args -> args @ [lambdaBody.Type]

                let baseFuncType =
                    collectedLambdaTypes
                    |> Utils.makeLambdaType

                let atomicFuncType =
                    collectedLambdaTypes
                    |> modifyFirstOfList (fun x -> typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x))
                    |> Utils.makeLambdaType

                // let atomicFInfo =
                //     Utils.getMethodInfoOfLambda <@ atomicF @>
                //     |> fun mInfo ->
                //         mInfo.GetGenericMethodDefinition().MakeGenericMethod(
                //             collectedLambdaTypes
                //             |> fun types -> types.Head :: [Utils.makeLambdaType types.Tail]
                //             |> List.toArray
                //         )

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
                    let mutex = Utils.createRefCall <| Expr.Value 5
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
                        // Expr.DefaultValue <| getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicArgs,
                        Expr.Value 10,
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
                                    atomicXchg %%mutex 0
                                    flag <- false
                            barrier ()
                            // тут он как то криво тип выозвращаемого значения выводит (не выводит сосвсем)
                            (%%Expr.Var oldValueVar : int)
                        @@>
                    )

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

        | ExprShape.ShapeVar var -> Expr.Var var
        | ExprShape.ShapeLambda (var, lambda) ->
            Expr.Lambda(var, processAtomic lambda)
        | ExprShape.ShapeCombination (combo, exprs) ->
            ExprShape.RebuildShapeCombination(combo, List.map processAtomic exprs)
