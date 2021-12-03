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

open System.Collections.Generic
open Brahma.FSharp.OpenCL.AST
open System

type ArrayKind =
    | CPointer
    | CArrayDecl of size: int

type Flags() =
    member val enableAtomic = false with get, set
    member val enableFP64 = false with get, set

type TranslatorOption =
    | UseNativeBooleanType
    | BoolAsBit

type TranslationContext<'lang, 'vDecl> =
    {
        TopLevelVarsDecls: ResizeArray<'vDecl>
        UserDefinedTypes: HashSet<Type>
        // NOTE is it necessary  to have 3 dicts?
        TupleDecls: Dictionary<Type, StructType<'lang>>
        StructDecls: Dictionary<Type, StructType<'lang>>
        UnionDecls: Dictionary<Type, DiscriminatedUnionType<'lang>>

        VarDecls: ResizeArray<'vDecl>
        Namer: Namer
        ArrayKind: ArrayKind

        Flags: Flags
        TranslatorOptions: TranslatorOption list
    }

    static member Create([<ParamArray>] translatorOptions: TranslatorOption[]) =
        {
            TopLevelVarsDecls = ResizeArray<'vDecl>()
            UserDefinedTypes = HashSet<Type>()
            TupleDecls = Dictionary<Type, StructType<'lang>>()
            StructDecls = Dictionary<Type, StructType<'lang>>()
            UnionDecls = Dictionary<Type, DiscriminatedUnionType<'lang>>()

            VarDecls = ResizeArray<'vDecl>()
            Namer = Namer()
            ArrayKind = CPointer

            Flags = Flags()
            TranslatorOptions = translatorOptions |> Array.toList
        }

    member this.WithNewLocalContext() =
        { this with
            VarDecls = ResizeArray()
            Namer = Namer()
            ArrayKind = CPointer
        }

type TargetContext = TranslationContext<Lang, Statement<Lang>>

[<AutoOpen>]
module Translation =
    let translation = StateBuilder<TargetContext>()
