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
open System

exception InvalidKernelException of string

type Flags() =
    member val enableAtomic = false with get, set
    member val enableFP64 = false with get, set

type TranslatorOption =
    | UseNativeBooleanType
    | BoolAsBit

type TargetContext<'lang, 'vDecl>([<ParamArray>] translatorOptions: TranslatorOption[]) =
    let mutable topLevelVarsDeclarations = ResizeArray<'vDecl>()
    let mutable varDecls = ResizeArray<'vDecl>()
    let mutable flags = Flags()
    let mutable namer = Namer()
    let mutable tn = 0

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
        let context = TargetContext(translatorOptions)

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

type TranslationContext = TargetContext<Lang, Statement<Lang>>

[<AutoOpen>]
module StateBuilder =
    let translation = StateBuilder<TranslationContext>()
    let state = StateBuilder<Map<Var, Var>>()

    let (>>=) = State.(>>=)
