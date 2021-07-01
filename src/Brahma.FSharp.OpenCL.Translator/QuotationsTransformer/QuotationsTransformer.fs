// Copyright (c) 2013 Semyon Grigorev <rsdpisuy@gmail.com>
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

module Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Collections

open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.MutableVarsToRef
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.PrintfReplacer
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LetVarAbstracter
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LambdaLifting.LambdaLifting
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.UniqueVarRenaming
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.MutableVarsInClosureCollector

let mainKernelName = "brahmaKernel"

let preprocessQuotation expr =
    replacePrintf expr

/// Returns kernel and other methods
let quotationTransformer expr translatorOptions : Expr * List<Method> =
    let preprocessedExpr =
        expr
        |> replacePrintf
        |> makeVarNameUnique
        |> varDefsToLambda
        |> fun expr ->
            let mutableVarsInClosure = collectMutableVarsInClosure expr
            varsToRefsWithPredicate mutableVarsInClosure.Contains expr

    let lastExpr, methods = lambdaLifting preprocessedExpr
    lastExpr, methods
