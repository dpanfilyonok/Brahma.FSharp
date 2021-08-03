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

open FSharpx.Collections
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Brahma.FSharp.OpenCL.Translator.TypeReflection
open FSharp.Core.LanguagePrimitives

#nowarn "3390"

type FSQuotationToOpenCLTranslator() =
    let mainKernelName = "brahmaKernel"

    // TODO delete this
    // http://www.fssnip.net/bx/title/Expanding-quotations
    /// The parameter 'vars' is an immutable map that assigns expressions to variables
    /// (as we recursively process the tree, we replace all known variables)
    // let rec expand vars expr =

    //     // First recursively process & replace variables
    //     let expanded =
    //         match expr with
    //         // If the variable has an assignment, then replace it with the expression
    //         | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]
    //         // Apply 'expand' recursively on all sub-expressions
    //         | ExprShape.ShapeVar v -> Expr.Var v
    //         | Patterns.Call (body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
    //             let this =
    //                 match body with
    //                 | Some b -> Expr.Application(meth, b)
    //                 | _ -> meth

    //             let res =
    //                 Expr.Applications(this, [ for a in args -> [ a ] ])

    //             expand vars res
    //         | ExprShape.ShapeLambda (v, expr) -> Expr.Lambda(v, expand vars expr)
    //         | ExprShape.ShapeCombination (o, exprs) ->
    //             ExprShape.RebuildShapeCombination(o, List.map (expand vars) exprs)

    //     // After expanding, try reducing the expression - we can replace 'let'
    //     // expressions and applications where the first argument is lambda
    //     match expanded with
    //     | Patterns.Application (ExprShape.ShapeLambda (v, body), assign)
    //     | Patterns.Let (v, assign, body) -> expand (Map.add v (expand vars assign) vars) body
    //     | _ -> expanded

    let addReturn subAST =
        let rec adding (stmt: Statement<'lang>) =
            match stmt with
            | :? StatementBlock<'lang> as sb ->
                let listStaments = sb.Statements
                let lastStatement = listStaments.[listStaments.Count - 1]
                sb.Remove(listStaments.Count - 1)
                sb.Append(adding lastStatement)
                sb :> Statement<_>
            | :? Expression<'lang> as ex -> Return ex :> Statement<_>
            | :? IfThenElse<'lang> as ite ->
                let newThen =
                    adding ite.Then :?> StatementBlock<_>

                let newElse =
                    if Option.isNone ite.Else then
                        None
                    else
                        Some (adding ite.Else.Value :?> StatementBlock<_>)

                IfThenElse(ite.Condition, newThen, newElse) :> Statement<_>
            | _ -> failwithf "Unsupported statement to add Return: %A" stmt

        adding subAST

    // let brahmaDimensionsTypes = [ "_1d"; "_2d"; "_3d" ]
    // let brahmaDimensionsTypesPrefix = "brahma.opencl."

    let brahmaDimensionsTypes =
        ["_1d"; "_2d"; "_3d"]
        |> List.map (fun s -> "brahma.opencl." + s)

    /// <param name="methodArgumentVarsList">Arguments for each method</param>
    /// <param name="methodVarList">Methods to vars bindings</param>
    let buildFullAst
        (methodArgumentVarsList: ResizeArray<Var list>)
        (methodVarList: ResizeArray<Var>)
        topDefTypes
        (partialAstList: ResizeArray<_>)
        (contextList: ResizeArray<TargetContext<_,_>>)
        (kernelArgumentsNames: string list)
        (localVarsNames: string list)
        (atomicRefArgQualifiers: System.Collections.Generic.Dictionary<Var, AddressSpaceQualifier<Lang>>) =
        // extract pragmas

        let listCLFun = ResizeArray()

        for i in 0 .. methodArgumentVarsList.Count - 1 do
            // фргументы функции
            let formalArgs =
                if methodVarList.[i] |> atomicRefArgQualifiers.ContainsKey then
                    let qual = atomicRefArgQualifiers.[methodVarList.[i]]
                    methodArgumentVarsList.[i]
                    |> List.mapi
                        (fun i variable ->
                            let vType = Type.translate variable.Type true None contextList.[i]
                            let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                            if i = 0 then
                                declSpecs.AddressSpaceQualifier <- qual


                            FunFormalArg(declSpecs, variable.Name)
                        )
                else
                methodArgumentVarsList.[i]
                // обрабатываем все кроме аргументов типа ndrange
                |> List.filter
                    (fun (variable: Var) ->
                        brahmaDimensionsTypes
                        |> (not << List.contains (variable.Type.FullName.ToLowerInvariant()))
                    )
                |> List.map
                    (fun variable ->
                        let vType = Type.translate variable.Type true None contextList.[i]
                        let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                        // по сути мы посто взяли тиена аргументов у кернела и отметили их глобал
                        // этими же именами названы и соответствующие аргументы в функциях, поэтому все норм
                        if
                            vType :? RefType<_> &&
                            kernelArgumentsNames |> List.contains variable.Name
                        then
                            declSpecs.AddressSpaceQualifier <- Global
                        elif
                            vType :? RefType<_> &&
                            localVarsNames |> List.contains variable.Name
                        then
                            declSpecs.AddressSpaceQualifier <- Local

                        FunFormalArg(declSpecs, variable.Name)
                    )

            // биндинг переменной
            let funVar = methodVarList.[i]

            // тип возвращаемого знчения
            let retFunType =
                // если текущий метод  не ядро
                if i <> methodArgumentVarsList.Count - 1 then
                    Type.translate funVar.Type false None contextList.[i]
                else
                    PrimitiveType Void :> Type<_>

            // ??? тело метода?
            let partAST =
                if (retFunType :?> PrimitiveType<_>).Type <> Void then
                    addReturn partialAstList.[i]
                else
                    partialAstList.[i]

            // спеки для функции
            let declSpecs =
                let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType)

                // if isKernel
                if funVar.Name = mainKernelName then
                    declSpecs.FunQual <- Some Kernel

                declSpecs

            // wtf?? это просто тело функции?
            let mainKernelFun =
                FunDecl(declSpecs, funVar.Name, formalArgs, partAST)

            let pragmas =
                let pragmas = ResizeArray()

                if contextList.[i].Flags.enableAtomic then
                    pragmas.Add(CLPragma CLGlobalInt32BaseAtomics :> ITopDef<_>)
                    pragmas.Add(CLPragma CLLocalInt32BaseAtomics :> ITopDef<_>)

                if contextList.[i].Flags.enableFP64 then
                    pragmas.Add(CLPragma CLFP64)

                List.ofSeq pragmas

            let topLevelVarDecls =
                contextList.[i].TopLevelVarsDeclarations
                |> Seq.cast<_>
                |> List.ofSeq

            let translatedTuples =
                contextList.[i].TupleList
                |> Seq.cast<_>
                |> List.ofSeq

            // почему тут mainKernelFun припысываем всегда?
            listCLFun.AddRange pragmas
            listCLFun.AddRange translatedTuples
            listCLFun.AddRange topLevelVarDecls
            listCLFun.AddRange topDefTypes
            listCLFun.Add mainKernelFun

        AST <| List.ofSeq listCLFun

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

        let kernelMethod =
            Method(Var(mainKernelName, kernelExpr.Type), kernelExpr)

        // глобальные переменные
        let kernelArgumentsNames =
            kernelMethod.FunExpr
            |> Utils.collectLambdaArguments
            |> List.map (fun var -> var.Name)

        // локальные переменные
        let localVarsNames =
            kernelExpr
            |> Utils.collectLocalVars
            |> List.map (fun var -> var.Name)

        // ядро + функции
        let methods =
            methods @ [ kernelMethod ] |> ResizeArray.ofList

        let atomicRefArgQualifiers = System.Collections.Generic.Dictionary<Var, AddressSpaceQualifier<Lang>>()
        methods
        |> Seq.iter (fun method ->
            let rec go expr =
                match expr with
                | DerivedPatterns.Applications
                    (
                        Patterns.Var funcVar,
                        applicationArgs
                    )
                    when funcVar.Name.StartsWith "atomic" ->

                    match applicationArgs.[0].[0] with
                    | Patterns.Var var ->
                        if kernelArgumentsNames |> List.contains var.Name then
                            atomicRefArgQualifiers.Add(var, Global)
                        elif localVarsNames |> List.contains var.Name then
                            atomicRefArgQualifiers.Add(var, Local)
                        else
                            failwith "F"
                    | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [Patterns.Var arrayVar; idx]) ->
                        if kernelArgumentsNames |> List.contains arrayVar.Name then
                            atomicRefArgQualifiers.Add(arrayVar, Global)
                        elif localVarsNames |> List.contains arrayVar.Name then
                            atomicRefArgQualifiers.Add(arrayVar, Local)
                        else
                            failwith "F"
                    | _ -> failwith "F"

                | ExprShape.ShapeVar var -> ()
                | ExprShape.ShapeLambda (var, lambda) ->
                    go lambda
                | ExprShape.ShapeCombination (combo, exprs) ->
                    List.iter go exprs

            go method.FunExpr
        )

        let translateMethod expr =
            match expr with
            // собираем параметры верхнего уровня
            | DerivedPatterns.Lambdas (args, body) ->
                let args = List.collect id args
                let body =
                    let (b, context) =
                        let clonedContext = context.Clone()

                        clonedContext.Namer.LetIn()
                        args |> List.iter (fun v -> clonedContext.Namer.AddVar v.Name)

                        Body.translate body clonedContext

                    match b with
                    | :? StatementBlock<Lang> as sb -> sb
                    | :? Statement<Lang> as s -> StatementBlock <| ResizeArray [s]
                    | _ -> failwithf "Incorrect function body: %A" b
                    , context

                args, body
            | _ -> failwithf "Incorrect OpenCL quotation: %A" expr

        // аргументы методов
        let listPartsASTMethodArgumentVars = ResizeArray()

        // биндинг метода к переменной
        let listPartsASTMethodVar = ResizeArray()

        // тела методов
        let listPartsASTMethodBody = ResizeArray()

        // контексты методов
        let listPartsASTContext = ResizeArray()

        for method in methods do
            let (vars, (partialAst, context)) = translateMethod method.FunExpr
            listPartsASTMethodArgumentVars.Add(vars)
            listPartsASTMethodVar.Add(method.FunVar)
            listPartsASTMethodBody.Add(partialAst :> Statement<_>)
            listPartsASTContext.Add(context)

        let ast =
            buildFullAst
                listPartsASTMethodArgumentVars
                listPartsASTMethodVar
                translatedTypes
                listPartsASTMethodBody
                listPartsASTContext
                kernelArgumentsNames
                localVarsNames
                atomicRefArgQualifiers

        ast, methods

    member this.Translate(qExpr, translatorOptions: TranslatorOption list) =
        let lockObject = obj ()

        lock lockObject <| fun () ->
            translate qExpr translatorOptions
