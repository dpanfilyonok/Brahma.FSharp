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

namespace rec Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Brahma.FSharp.OpenCL.Translator.TypeReflection

#nowarn "3390"

type FSQuotationToOpenCLTranslator() =
    let mainKernelName = "brahmaKernel"

    let collectData (expr: Expr) (functions: (Var * Expr) list) =
        // глобальные переменные
        let kernelArgumentsNames =
            expr
            |> Utils.collectLambdaArguments
            |> List.map (fun var -> var.Name)

        // локальные переменные
        let localVarsNames =
            expr
            |> Utils.collectLocalVars
            |> List.map (fun var -> var.Name)

        let atomicApplicationsInfo =
            let atomicPointerArgQualifiers = System.Collections.Generic.Dictionary<Var, AddressSpaceQualifier<Lang>>()

            let rec go expr =
                match expr with
                | DerivedPatterns.Applications
                    (
                        Patterns.Var funcVar,
                        [mutex] :: [DerivedPatterns.SpecificCall <@ ref @> (_, _, [Patterns.ValidVolatileArg var])] :: _
                    )
                    when funcVar.Name.StartsWith "atomic" ->

                    if kernelArgumentsNames |> List.contains var.Name then
                        atomicPointerArgQualifiers.Add(funcVar, Global)
                    elif localVarsNames |> List.contains var.Name then
                        atomicPointerArgQualifiers.Add(funcVar, Local)
                    else
                        failwith "Atomic pointer argument should be from local or global memory only"

                | ExprShape.ShapeVar _ -> ()
                | ExprShape.ShapeLambda (_, lambda) -> go lambda
                | ExprShape.ShapeCombination (_, exprs) -> List.iter go exprs

            functions
            |> List.map snd
            |> fun tail -> expr :: tail
            |> Seq.iter go

            atomicPointerArgQualifiers
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq

        kernelArgumentsNames, localVarsNames, atomicApplicationsInfo

    let constructMethods (expr: Expr) (functions: (Var * Expr) list) (atomicApplicationsInfo: Map<Var, AddressSpaceQualifier<Lang>>) context =
        let kernelFunc = KernelFunc(Var(mainKernelName, expr.Type), expr, context) :> Method |> List.singleton

        let methods =
            functions
            |> List.map (fun (var, expr) ->
                match atomicApplicationsInfo |> Map.tryFind var with
                | Some qual -> AtomicFunc(var, expr, qual, context) :> Method
                | None -> Function(var, expr, context) :> Method
            )

        methods @ kernelFunc

    let translate qExpr translatorOptions =
        let qExpr' = preprocessQuotation qExpr

        let structs = collectStructs qExpr'
        let unions = collectDiscriminatedUnions qExpr

        let context = TargetContext()

        let translatedStructs =
            Type.translateStructDecls structs context
            |> List.map (fun x -> x :> ITopDef<_>)

        let translatedUnions =
            Type.translateDiscriminatedUnionDecls unions context
            |> List.map (fun x -> x :> ITopDef<_>)

        let translatedTypes =
            List.concat [ translatedStructs
                          translatedUnions ]

        // TODO: Extract quotationTransformer to translator
        let (kernelExpr, functions) = transformQuotation qExpr' translatorOptions
        let (globalVars, localVars, atomicApplicationsInfo) = collectData kernelExpr functions
        let methods = constructMethods kernelExpr functions atomicApplicationsInfo context

        let listCLFun = ResizeArray []
        for method in methods do
            listCLFun.AddRange(method.Translate(globalVars, localVars, translatedTypes))

        AST <| List.ofSeq listCLFun,
        methods
        |> List.find (fun method -> method :? KernelFunc)
        |> fun kernel -> kernel.FunExpr

    member this.Translate(qExpr, translatorOptions: TranslatorOption list) =
        let lockObject = obj ()

        lock lockObject <| fun () ->
            translate qExpr translatorOptions
