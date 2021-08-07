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
open FSharp.Core.LanguagePrimitives

#nowarn "3390"

type FSQuotationToOpenCLTranslator() =
    let mainKernelName = "brahmaKernel"

    let collectMetadata (expr: Expr, functions: (Var * Expr) list) =
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
            let atomicRefArgQualifiers = System.Collections.Generic.Dictionary<Var, AddressSpaceQualifier<Lang>>()

            let rec go expr =
                match expr with
                | DerivedPatterns.Applications
                    (
                        Patterns.Var funcVar,
                        applicationArgs
                    )
                    when funcVar.Name.StartsWith "atomic" ->

                    // сначала идут переменные из замыкания -- неизвестно сколько
                    // потом переменные непосредственно функции
                    // ориентируемся по ref переменной -- считаем, что она должна быть только 1 берем первую
                    let refVar =
                        applicationArgs
                        |> List.collect id
                        |> List.tryFind (function | DerivedPatterns.SpecificCall <@ ref @> _ -> true | _ -> false)
                        |> Option.defaultWith (fun () -> failwith "Atomic application should have at least one ref argument")

                    match refVar with
                    | DerivedPatterns.SpecificCall <@ ref @>
                        (
                            _,
                            _,
                            [Patterns.Var var]
                        )
                    | DerivedPatterns.SpecificCall <@ ref @>
                        (
                            _,
                            _,
                            [DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var var; _])]
                        ) ->

                        if kernelArgumentsNames |> List.contains var.Name then
                            atomicRefArgQualifiers.Add(funcVar, Global)
                        elif localVarsNames |> List.contains var.Name then
                            atomicRefArgQualifiers.Add(funcVar, Local)
                        else
                            failwith "Atomic pointer argument should be from local or global memory only"

                    | _ -> failwith "Atomic pointer argument should be 'var' or 'var.[idx]'"

                | ExprShape.ShapeVar _ -> ()
                | ExprShape.ShapeLambda (_, lambda) -> go lambda
                | ExprShape.ShapeCombination (_, exprs) -> List.iter go exprs

            functions
            |> Seq.iter (snd >> go)

            go expr

            atomicRefArgQualifiers
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq

        let main = KernelFunc(Var(mainKernelName, expr.Type), expr) :> Method |> List.singleton

        let funcs =
            functions
            |> List.filter (fun (v, _) -> not <| atomicApplicationsInfo.ContainsKey(v))
            |> List.map (fun (v, expr) -> Function(v, expr) :> Method)

        let atomicFuncs =
            functions
            |> List.choose (fun (var, expr) ->
                match atomicApplicationsInfo |> Map.tryFind var with
                | Some qual -> Some (AtomicFunc(var, expr, qual) :> Method)
                | None -> None
            )

        funcs @ atomicFuncs @ main, kernelArgumentsNames, localVarsNames

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
        let (kernelExpr, methods) =
            transformQuotation qExpr' translatorOptions

        let (methods, globalVars, localVars) = collectMetadata (kernelExpr, methods)
        let listCLFun = ResizeArray(translatedTypes)
        methods
        |> List.iter (fun m -> listCLFun.AddRange(m.Translate(globalVars, localVars)))

        (AST <| List.ofSeq listCLFun), (methods |> List.find (fun m -> m :? KernelFunc)).FunExpr

    member this.Translate(qExpr, translatorOptions: TranslatorOption list) =
        let lockObject = obj ()

        lock lockObject <| fun () ->
            translate qExpr translatorOptions
