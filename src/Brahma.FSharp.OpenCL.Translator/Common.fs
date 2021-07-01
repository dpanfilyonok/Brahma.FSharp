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

[<AutoOpen>]
module Brahma.FSharp.OpenCL.Translator.Common

open Microsoft.FSharp.Quotations
open System.Collections.Generic
open Brahma.FSharp.OpenCL.AST

type Flags () =
    member val enableAtomic = false with get, set
    member val enableFP64 = false with get, set

type TranslatorOption =
    | BoolAsBit

type TargetContext<'lang, 'vDecl>() =
    let mutable topLevelVarsDeclarations = new ResizeArray<'vDecl>()
    let varDecls = new ResizeArray<'vDecl>()
    let mutable flags = Flags()
    let mutable namer = Namer()
    let mutable tn = 0

    let mutable translatorOptions = ResizeArray<TranslatorOption>()
    member val tupleDecls = Dictionary<string, int>()
    member val tupleList = List<StructType<Lang>>()
    member val UserDefinedTypes = ResizeArray<System.Type>()
    member val InLocal = false with get, set

    member val UserDefinedStructsOpenCLDeclaration = Dictionary<string, StructType<'lang>>()
    member val UserDefinedUnionsOpenCLDeclaration = Dictionary<string, DiscriminatedUnionType<'lang>>()

    member this.tupleNumber
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
    member this.TranslatorOptions with get() = translatorOptions
    member this.Namer
        with get() = namer
        and set v = namer <- v

    member this.Clone () =
        let c = TargetContext<_,_>()

        c.UserDefinedTypes.AddRange this.UserDefinedTypes

        for x in this.UserDefinedStructsOpenCLDeclaration do
            c.UserDefinedStructsOpenCLDeclaration.Add (x.Key,x.Value)
        for x in this.UserDefinedUnionsOpenCLDeclaration do
            c.UserDefinedUnionsOpenCLDeclaration.Add (x.Key,x.Value)
        for x in this.tupleDecls do
            c.tupleDecls.Add(x.Key,x.Value)
        for x in this.tupleList do
            c.tupleList.Add(x)
        c.tupleNumber <- this.tupleNumber
        c.Flags.enableFP64 <- this.Flags.enableFP64
        c.TranslatorOptions.AddRange translatorOptions
        c

type Method(var: Var, expr: Expr) =
    let funVar = var
    let funExpr = expr

    member this.FunVar: Var =
        funVar
    member this.FunExpr =
        funExpr
