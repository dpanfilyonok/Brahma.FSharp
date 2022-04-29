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

type Flag =
    | EnableAtomic
    | EnableFP64

type TranslationContext<'lang, 'vDecl> =
    {
        // translator scope
        TranslatorOptions: TranslatorOptions

        // kernel scope
        CStructDecls: Dictionary<Type, StructType<'lang>>
        StructInplaceCounter: Dictionary<string, int>
        TopLevelVarsDecls: ResizeArray<'vDecl>
        Flags: HashSet<Flag>

        // function scope
        VarDecls: ResizeArray<'vDecl>
        Namer: Namer

        // specific scope
        ArrayKind: ArrayKind
    }

    static member Create(options) =
        {
            TranslatorOptions = options

            CStructDecls = Dictionary<Type, StructType<'lang>>()
            StructInplaceCounter = Dictionary<string, int>()
            TopLevelVarsDecls = ResizeArray<'vDecl>()
            Flags = HashSet()

            VarDecls = ResizeArray<'vDecl>()
            Namer = Namer()

            ArrayKind = CPointer
        }

    member this.WithNewLocalContext() =
        { this with
            VarDecls = ResizeArray()
            Namer = Namer()
        }

type TargetContext = TranslationContext<Lang, Statement<Lang>>

[<AutoOpen>]
module Translation =
    let translation = StateBuilder<TargetContext>()
