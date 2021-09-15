// Copyright (c) 2012, 2013 Semyon Grigorev <rsdpisuy@gmail.com>
// All rights reserved.
//
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.

namespace Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open System.Collections.Generic
open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Reflection

exception InvalidKernelException of string

type Flags() =
    member val enableAtomic = false with get, set
    member val enableFP64 = false with get, set

type TranslatorOption =
    | BoolAsBit

type TargetContext<'lang, 'vDecl>() =
    let mutable topLevelVarsDeclarations = ResizeArray<'vDecl>()
    let mutable varDecls = ResizeArray<'vDecl>()
    let mutable flags = Flags()
    let mutable namer = Namer()
    let mutable tn = 0
    let mutable translatorOptions = ResizeArray<TranslatorOption>()

    member val TupleDecls = Dictionary<string, int>()
    member val TupleList = List<StructType<Lang>>()
    member val UserDefinedTypes = ResizeArray<System.Type>()
    member val InLocal = false with get, set
    member val UserDefinedStructsOpenCLDeclaration = Dictionary<string, StructType<'lang>>()
    member val UserDefinedUnionsOpenCLDeclaration = Dictionary<string, DiscriminatedUnionType<'lang>>()

    member this.TupleNumber
        with get() = tn
        and set tn2 = tn <- tn2

    member this.TopLevelVarsDeclarations
        with get() = topLevelVarsDeclarations
        and  set v = topLevelVarsDeclarations <- v

    member this.VarDecls
        with get() = varDecls

    member this.Flags
        with get() = flags
        and set v = flags <- v

    member this.TranslatorOptions
        with get() = translatorOptions

    member this.Namer
        with get() = namer
        and set v = namer <- v

    // NOTE is it really clone context (is it fully clone)
    member this.Clone() =
        let context = TargetContext()

        context.UserDefinedTypes.AddRange this.UserDefinedTypes

        for x in this.UserDefinedStructsOpenCLDeclaration do
            context.UserDefinedStructsOpenCLDeclaration.Add (x.Key,x.Value)
        for x in this.UserDefinedUnionsOpenCLDeclaration do
            context.UserDefinedUnionsOpenCLDeclaration.Add (x.Key,x.Value)
        for x in this.TupleDecls do
            context.TupleDecls.Add(x.Key,x.Value)
        for x in this.TupleList do
            context.TupleList.Add(x)
        context.TupleNumber <- this.TupleNumber

        context.Flags.enableFP64 <- this.Flags.enableFP64
        context.Flags.enableAtomic <- this.Flags.enableAtomic
        context.TranslatorOptions.AddRange translatorOptions
        context

[<AutoOpen>]
module Extensions =
    type Expr with
        /// Builds an expression that represents the lambda
        static member Lambdas(args: Var list list, body: Expr) =
            let mkRLinear mk (vs, body) = List.foldBack (fun v acc -> mk (v, acc)) vs body

            let mkTupledLambda (args, body) =
                match args with
                | [x] -> Expr.Lambda(x, body)
                | [] -> Expr.Lambda(Var("unitVar", typeof<unit>), body)
                | _ ->
                    let tupledArg =
                        Var(
                            "tupledArg",
                            FSharpType.MakeTupleType(args |> List.map (fun v -> v.Type) |> List.toArray)
                        )

                    Expr.Lambda(
                        tupledArg,
                        (args, [0 .. args.Length - 1], body)
                        |||> List.foldBack2
                            (fun var idxInTuple letExpr ->
                                Expr.Let(
                                    var,
                                    Expr.TupleGet(Expr.Var tupledArg, idxInTuple),
                                    letExpr
                                )
                            )
                    )

            mkRLinear mkTupledLambda (args, body)

module internal Anchors =
    let _localID0 = Unchecked.defaultof<int>

type State<'state, 'result> = State of ('state -> 'result * 'state)

module State =
    let run state (State f) =
        f state

    let exec state (State f) =
        snd (f state)

    let eval state (State f) =
        fst (f state)

    let return' x = State <| fun state ->
        (x, state)

    let (>>=) x f = State <| fun state ->
        let (y, state') = run state x
        run state' (f y)

    let get = State (fun s -> s, s)

    let put newState = State <| fun _ ->
        (), newState

    // modify state
    let modify f =
        get >>= (f >> put)

    // apply f to state to produce value
    let gets f =
        get >>= (f >> return')

    let map f s = State <| fun state ->
        let (x, state) = run state s
        f x, state

    let using f x = State <| fun state ->
        eval (f state) x, state

    let collect (list: State<'s, 'a> list) =
        list
        |> List.fold
            (fun state elem ->
                state >>= fun state ->
                elem >>= fun elem ->
                return' (elem :: state)
            ) (return' List.empty)
        |> fun args -> map List.rev args

type StateBuilder() =
    member this.Bind(x, f) = State.(>>=) x f
    member this.Return x = State.return' x
    member this.ReturnFrom x = x
    member this.Zero() = State.return' ()

    member this.Combine(x1, x2) =
        State <| fun context ->
            let (_, context) = State.run context x1
            State.run context x2

    member this.Delay f = f ()

    member this.For(seq, f) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine(x1, x2))

    member this.While(f, x) =
        if f () then
            this.Combine(x, this.While(f, x))
        else this.Zero()

[<AutoOpen>]
module StateBuilder =
    let state = StateBuilder()
    let (>>=) = State.(>>=)
