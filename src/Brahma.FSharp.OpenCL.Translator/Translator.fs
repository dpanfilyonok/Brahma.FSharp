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
open Brahma.FSharp.OpenCL.Translator.QuotationsTransformer
open Brahma.FSharp.OpenCL.QuotationsTransformer.Utils.Common
open Brahma.FSharp.OpenCL.Translator.TypeReflection

type FSQuotationToOpenCLTranslator() =
    /// The parameter 'vars' is an immutable map that assigns expressions to variables
    /// (as we recursively process the tree, we replace all known variables)
    let rec expand vars expr =

      // First recursively process & replace variables
      let expanded =
        match expr with
        // If the variable has an assignment, then replace it with the expression
        | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]
        // Apply 'expand' recursively on all sub-expressions
        | ExprShape.ShapeVar v -> Expr.Var v
        | Patterns.Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
            let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
            let res = Expr.Applications(this, [ for a in args -> [a]])
            expand vars res
        | ExprShape.ShapeLambda(v, expr) ->
            Expr.Lambda(v, expand vars expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            ExprShape.RebuildShapeCombination(o, List.map (expand vars) exprs)

      // After expanding, try reducing the expression - we can replace 'let'
      // expressions and applications where the first argument is lambda
      match expanded with
      | Patterns.Application(ExprShape.ShapeLambda(v, body), assign)
      | Patterns.Let(v, assign, body) ->
          expand (Map.add v (expand vars assign) vars) body
      | _ -> expanded

    let addReturn subAST =
        let rec adding (stmt:Statement<'lang>) =
            match stmt with
            | :? StatementBlock<'lang> as sb ->
                let listStaments = sb.Statements
                let lastStatement = listStaments.[listStaments.Count - 1]
                sb.Remove (listStaments.Count - 1)
                sb.Append(adding lastStatement)
                sb :> Statement<_>
            | :? Expression<'lang> as ex -> (new Return<_>(ex)) :> Statement<_>
            | :? IfThenElse<'lang> as ite ->
                let newThen = (adding (ite.Then)) :?> StatementBlock<_>
                let newElse =
                    if(ite.Else = None) then
                        None
                    else
                        Some((adding (ite.Else.Value)) :?> StatementBlock<_> )
                (IfThenElse<_>(ite.Condition,newThen, newElse)) :> Statement<_>
            | _ -> failwithf "Unsupported statement to add Return: %A" stmt

        adding subAST

    let brahmaDimensionsTypes = ["_1d";"_2d";"_3d"]
    let brahmaDimensionsTypesPrefix = "brahma.fsharp.opencl."
    let bdts = brahmaDimensionsTypes |> List.map (fun s -> brahmaDimensionsTypesPrefix + s)

    let buildFullAst
      (methodArgumentVarsList: ResizeArray<_>) (methodVarList: ResizeArray<Var>) types (partialAstList: ResizeArray<_>)
      (contextList:ResizeArray<TargetContext<_,_>>) (kernelArgumentsNames: list<string>) =
        // extract pragmas

        let mutable listCLFun = []
        for i in 0..(methodArgumentVarsList.Count-1) do
            let formalArgs =
                methodArgumentVarsList.[i]
                |> List.filter (fun (v:Var) -> bdts |> List.exists((=) (v.Type.FullName.ToLowerInvariant())) |> not)
                |> List.map
                    (fun v ->
                        let t = Type.Translate v.Type true None contextList.[i]
                        let declSpecs = DeclSpecifierPack<_>(typeSpec=t)
                        if t :? RefType<_> && List.contains v.Name kernelArgumentsNames
                        then
                            declSpecs.AddressSpaceQual <- Global
                        FunFormalArg<_>(declSpecs, v.Name))

            let funVar: Var = methodVarList.[i]
            let mutable retFunType = PrimitiveType<_>(Void) :> Type<_>
            if i <> methodArgumentVarsList.Count-1 then
                let funType = funVar.Type
                retFunType <- Type.Translate funType  false  None (contextList.[i])
            let typeRet = retFunType :?> PrimitiveType<_>
            let partAST =
                if typeRet.Type <> PTypes.Void
                then addReturn partialAstList.[i]
                else partialAstList.[i]

            let isKernel = (funVar.Name = mainKernelName)

            let declSpecs = DeclSpecifierPack<_>(typeSpec=retFunType)
            if isKernel then declSpecs.FunQual <- Some Kernel
            let mainKernelFun = FunDecl<_>(declSpecs, funVar.Name, formalArgs, partAST)

            let pragmas =
                let res = ResizeArray<_>()
                if contextList.[i].Flags.enableAtomic
                then
                    res.Add(CLPragma<_>(CLGlobalInt32BaseAtomics) :> TopDef<_>)
                    res.Add(CLPragma<_>(CLLocalInt32BaseAtomics) :> TopDef<_>)
                if contextList.[i].Flags.enableFP64
                then res.Add(CLPragma<_>(CLFP64))
                List.ofSeq res
            let topLevelVarDecls = contextList.[i].TopLevelVarsDeclarations |> Seq.cast<_> |> List.ofSeq
            let translatedTuples = contextList.[i].tupleList |> Seq.cast<_> |> List.ofSeq
            listCLFun <- listCLFun@pragmas@translatedTuples@topLevelVarDecls@types@[mainKernelFun]
        AST<_>(listCLFun)

    let translate qExpr translatorOptions =
        let qExpr' = preprocessQuotation qExpr

        let structs = CollectStructs qExpr'
        let unions = CollectDiscriminatedUnions qExpr

        let context = TargetContext<_,_>()

        let translatedStructs =
            Type.TranslateStructDecls structs context
            |> List.map (fun x -> x :> TopDef<_>)
        let translatedUnions =
            Type.translateDiscriminatedUnionDecls unions context
            |> List.map (fun x -> x :> TopDef<_>)
        let translatedTypes = List.concat [translatedStructs; translatedUnions]

        // TODO: Extract quotationTransformer to translator
        let kernelExpr, methods = quotationTransformer qExpr' translatorOptions
        let kernelMethod = Method(Var(mainKernelName, kernelExpr.Type), kernelExpr)

        let kernelArgumentsNames =
            kernelMethod.FunExpr
            |> collectLambdaArguments
            |> List.map (fun var -> var.Name)

        let methods = methods @ [kernelMethod] |> ResizeArray.ofList
        let rec go expr vars  =
            match expr with
            | Patterns.Lambda (v, body) -> go body (v::vars)
            | e ->
                let body =
                    let b,context =
                        let c = context.Clone()

                        c.Namer.LetIn()
                        vars |> List.iter (fun v -> c.Namer.AddVar v.Name)

                        Body.Translate e c
                    match b  with
                    | :? StatementBlock<Lang> as sb -> sb
                    | :? Statement<Lang> as s -> StatementBlock<_>(ResizeArray<_>([s]))
                    | _ -> failwithf "Incorrect function body: %A" b
                    , context
                vars, body
            | x -> "Incorrect OpenCL quotation: " + string x |> failwith

        let listPartsASTMethodArgumentVars = ResizeArray<_>()
        let listPartsASTMethodVar = ResizeArray<_>()
        let listPartsASTMethodBody = ResizeArray<_>()
        let listPartsASTContext = ResizeArray<_>()

        for method in methods do
            let vars, (partialAst, context) = go method.FunExpr []
            listPartsASTMethodArgumentVars.Add(List.rev vars)
            listPartsASTMethodVar.Add(method.FunVar)
            listPartsASTMethodBody.Add(partialAst :> Statement<_>)
            listPartsASTContext.Add(context)

        let AST = buildFullAst listPartsASTMethodArgumentVars listPartsASTMethodVar translatedTypes listPartsASTMethodBody listPartsASTContext kernelArgumentsNames
        AST, methods

    member this.Translate qExpr translatorOptions =
        let lObj = "str"
        lock lObj (fun _ ->
            let ast, newQExpr = translate qExpr translatorOptions
            ast, newQExpr)
