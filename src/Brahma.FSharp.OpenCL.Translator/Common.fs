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

    // TODO is it really clone context (is it fully clone)
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
        // TODO why only enableFP64 clones
        context.Flags.enableFP64 <- this.Flags.enableFP64
        context.TranslatorOptions.AddRange translatorOptions
        context

type Method(var: Var, expr: Expr) =
    member this.FunVar = var
    member this.FunExpr = expr

    override this.ToString() =
        sprintf "%A\n%A" var expr

type Context = TargetContext<Lang, Statement<Lang>>
type TranslationContext<'a> = TranslationContext of (Context -> 'a * Context)

module Translator =
    let run context (TranslationContext f) =
        f context

    let exec context (TranslationContext f) =
        snd (f context)

    let eval context (TranslationContext f) =
        fst (f context)

    let return' x = TranslationContext <| fun context ->
        (x, context)

    let (>>=) x f = TranslationContext <| fun context ->
        let (y, context') = run context x
        run context' (f y)

module TranslationContext =
    open Translator

    let get = TranslationContext (fun context -> context, context)

    let put newContext = TranslationContext <| fun _ ->
        (), newContext

    // modify state
    let modify f =
        get >>= (f >> put)

    // apply f to state to produce value
    let gets f =
        get >>= (f >> return')

    let map f s = TranslationContext <| fun context ->
        let (x, context') = run context s
        f x, context'

type TranslatorBuilder() =
    member this.Zero() = Translator.return' ()
    member this.Return x = Translator.return' x
    member this.ReturnFrom x = x
    member this.Bind(x, f) = Translator.(>>=) x f

    member this.Combine(x1, x2) =
        TranslationContext <| fun context ->
            let (_, context) = Translator.run context x1
            Translator.run context x2

    member this.Delay f = f ()

    member this.For(seq, f) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine(x1, x2))

    member this.While(f, x) =
        if f () then this.Combine(x, this.While(f, x))
        else this.Zero()

[<AutoOpen>]
module TranslatorBuilder =
    let translator = TranslatorBuilder()
    let (>>=) = Translator.(>>=)
