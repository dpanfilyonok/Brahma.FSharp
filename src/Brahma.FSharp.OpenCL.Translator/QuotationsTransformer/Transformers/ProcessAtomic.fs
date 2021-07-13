namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations
// open FSharp.Quotations.Patterns
// open FSharp.Quotations.DerivedPatterns
// open FSharp.Quotations.ExprShape

module ProcessAtomic =
    let inline atomicAdd p v = (+) p v
    let inline atomicSub p v = (-) p v
    let inline atomicInc p = inc p
    let inline atomicDec p = dec p
    let inline atomicXchg p v = xchg p v
    let inline atomicCmpxchg p cmp v = cmpxchg p cmp v
    let inline atomicMin p v = min p v
    let inline atomicMax p v = max p v
    let inline atomicAnd p v = (&&&) p v
    let inline atomicOr p v = (|||) p v
    let inline atomicXor p v = (^^^) p v
    let atomicF (f: obj) = f

    type Expr with
        // TODO
        static member Lambdas(args: Var list list, body: Expr) =
            let mkRLinear mk (vs, body) = List.foldBack (fun v acc -> mk(v, acc)) vs body

            let mkTupledLambda (args, body) =
                match args with
                // | [] -> Expr.Application (f, mkUnit())
                | [x] -> Expr.Lambda (x, body)
                // | _ -> Expr.Application (f, mkNewTuple args)
                | _ -> failwith "lol"

            let mkLambdas (args: Var list list) (body: Expr) =
                mkRLinear mkTupledLambda (args, body)

            mkLambdas args body

    // atomic (fun x y -> ...) a b
    // -> atomicAdd a b
    // -> let f = (fun x y -> ...) in
    //    let g x y = atomicF f x y in
    //    g a b
    let rec processA (expr: Expr) =
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
                let f = Var("f", typeof<_>)
                let g = Var("g", typeof<_>)
                Expr.Let(
                    f,
                    Expr.Lambdas(lambdaArgs, lambdaBody),
                    Expr.Let(
                        g,
                        Expr.Lambdas(
                            lambdaArgs,
                            Expr.Applications(
                                Expr.Call(Utils.getMethodInfoOfLambda <@@ atomicF @@>, [Expr.Var f]),
                                lambdaArgs |> List.map (List.map Expr.Var)
                            )
                        ),
                        Expr.Applications(
                            Expr.Var g,
                            applicationArgs
                        )
                    )
                )

        | ExprShape.ShapeVar var -> Expr.Var var
        | ExprShape.ShapeLambda (var, lambda) ->
            Expr.Lambda(var, processA lambda)
        | ExprShape.ShapeCombination (combo, exprs) ->
            ExprShape.RebuildShapeCombination(combo, List.map processA exprs)



