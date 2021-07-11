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

open System.Reflection
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Collections
open FSharpx.Collections
open System.Collections.Generic
open Brahma.FSharp.OpenCL.Translator.QuotationsTransformer.PrintfReplacer

module Body =
    let private clearContext (targetContext: TargetContext<'a, 'b>) =
        let context =
            TargetContext<'a, 'b>(
                Namer = targetContext.Namer,
                Flags = targetContext.Flags,
                TopLevelVarsDeclarations = targetContext.TopLevelVarsDeclarations,
                TupleNumber = targetContext.TupleNumber
            )

        for td in targetContext.TupleDecls do
            context.TupleDecls.Add(td.Key, td.Value)

        for t in targetContext.TupleList do
            context.TupleList.Add(t)

        for kvp in targetContext.UserDefinedStructsOpenCLDeclaration do
            context.UserDefinedStructsOpenCLDeclaration.Add(kvp.Key, kvp.Value)

        for kvp in targetContext.UserDefinedUnionsOpenCLDeclaration do
            context.UserDefinedUnionsOpenCLDeclaration.Add(kvp.Key, kvp.Value)

        context.UserDefinedTypes.AddRange(targetContext.UserDefinedTypes)

        context

    let rec private translateBinding (var: Var) newName (expr: Expr) (targetContext: TargetContext<_, _>) =
        let (body, tContext) = translateCond (*TranslateAsExpr*) expr targetContext
        let vType =
            match (body: Expression<_>) with
            | :? Const<Lang> as c -> c.Type
            | :? ArrayInitializer<Lang> as ai -> Type.translate var.Type false (Some ai.Length) targetContext
            | _ -> Type.translate var.Type false None targetContext

        VarDecl(vType, newName, Some body)

    and private translateListOfArgs (args: Expr list) targetContext =
        args
        |> List.fold
            (fun (res, context) arg ->
                let (r, context) = translateCond arg context
                r :: res, context
            ) ([], targetContext)
        |> fun (args, context) -> args |> List.rev, context

    and private translateCall exprOpt (mInfo: System.Reflection.MethodInfo) _args targetContext =
        let (args, tContext) = translateListOfArgs _args targetContext

        match mInfo.Name.ToLowerInvariant() with
        | "op_multiply" -> Binop(Mult, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_addition" -> Binop(Plus, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_division" -> Binop(Div, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_lessthan" -> Binop(Less, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_lessthanorequal" -> Binop(LessEQ, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_greaterthan" -> Binop(Great, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_greaterthanorequal" -> Binop(GreatEQ, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_equality" -> Binop(EQ, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_inequality" -> Binop(NEQ, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_subtraction" -> Binop(Minus, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_unarynegation" -> Unop(UOp.Minus, args.[0]) :> Statement<_>, tContext
        | "op_modulus" -> Binop(Remainder, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_bitwiseand" -> Binop(BitAnd, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_bitwiseor" -> Binop(BitOr, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_leftshift" -> Binop(LeftShift, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_rightshift" -> Binop(RightShift, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_booleanand" -> Binop(And, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_booleanor" -> Binop(Or, args.[0], args.[1]) :> Statement<_>, tContext
        | "op_lessbangplusgreater"
        | "op_lessbangplus" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_add", [Ptr args.[0]; args.[1]]) :> Statement<_>, tContext
        | "op_lessbangmunus"
        | "op_lessbangmunusgreater" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_sub", [Ptr args.[0]; args.[1]]) :> Statement<_>, tContext
        | "op_lessbanggreater"
        | "op_lessbang" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_xchg", [Ptr args.[0]; args.[1]]) :> Statement<_>, tContext
        | "amax"
        | "amaxr" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_max", [Ptr args.[0]; args.[1]]) :> Statement<_>, tContext
        | "amin"
        | "aminr" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_min", [Ptr args.[0]; args.[1]]) :> Statement<_>, tContext
        | "aincr"
        | "aincrr" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_inc", [Ptr args.[0]]) :> Statement<_>, tContext
        | "adecr"
        | "adecrr" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_dec", [Ptr args.[0]]) :> Statement<_>, tContext
        | "acompexch"
        | "acompexchr" ->
            tContext.Flags.enableAtomic <- true
            FunCall("atom_cmpxchg", [Ptr args.[0]; args.[1]; args.[2]]) :> Statement<_>, tContext

        | "todouble" -> Cast(args.[0], PrimitiveType Float) :> Statement<_>, tContext
        | "toint" -> Cast(args.[0], PrimitiveType Int) :> Statement<_>, tContext
        | "toint16" -> Cast(args.[0], PrimitiveType Short) :> Statement<_>, tContext
        | "tosingle" -> Cast(args.[0], PrimitiveType Float) :> Statement<_>, tContext
        | "tobyte" -> Cast(args.[0], PrimitiveType UChar) :> Statement<_>, tContext
        | "touint32" -> Cast(args.[0], PrimitiveType UInt) :> Statement<_>, tContext
        | "touint16" -> Cast(args.[0], PrimitiveType UShort) :> Statement<_>, tContext
        | "toint64" -> Cast(args.[0], PrimitiveType Long) :> Statement<_>, tContext
        | "touint64" -> Cast(args.[0], PrimitiveType ULong) :> Statement<_>, tContext
        | "acos"
        | "asin"
        | "atan"
        | "cos"
        | "cosh"
        | "exp"
        | "floor"
        | "log"
        | "log10"
        | "pow"
        | "sin"
        | "sinh"
        | "sqrt"
        | "tan"
        | "tanh" as fName ->
            if
                mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("System.Math") ||
                mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
            then
                FunCall(fName, args) :> Statement<_>, tContext
            else
                failwithf
                    "Seems, thet you use math function with name %s not from System.Math. or Microsoft.FSharp.Core.Operators"
                    fName
        | "abs" as fName ->
            if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                FunCall("fabs", args) :> Statement<_>, tContext
            else
                failwithf
                    "Seems, thet you use math function with name %s not from System.Math. or Microsoft.FSharp.Core.Operators"
                    fName
        | "powinteger" as fName ->
            if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                FunCall("powr", args) :> Statement<_>, tContext
            else
                failwithf
                    "Seems, thet you use math function with name %s not from System.Math. or Microsoft.FSharp.Core.Operators"
                    fName
        | "ref" -> Ptr args.[0] :> Statement<_>, tContext
        | "op_dereference" -> IndirectionOp args.[0] :> Statement<_>, tContext
        | "op_colonequals" ->
            Assignment(Property(PropertyType.VarReference(IndirectionOp args.[0])), args.[1]) :> Statement<_>,
            tContext
        | "setarray" ->
            let item = Item(args.[0], args.[1])
            Assignment(Property(PropertyType.Item item), args.[2]) :> Statement<_>, tContext
        | "getarray" -> Item(args.[0], args.[1]) :> Statement<_>, tContext
        | "not" -> Unop(UOp.Not, args.[0]) :> Statement<_>, tContext
        | "_byte" -> args.[0] :> Statement<_>, tContext
        | "barrier" -> Barrier() :> Statement<_>, tContext
        | "local" -> failwith "Calling the local function is allowed only at the top level of the let binding"
        | "arrayLocal" -> failwith "Calling the localArray function is allowed only at the top level of the let binding"
        | "zerocreate" ->
            let length =
                match args.[0] with
                | :? Const<Lang> as c -> int c.Val
                | other -> failwithf "Calling Array.zeroCreate with a non-const argument: %A" other
            ZeroArray length :> Statement<_>, tContext
        | "fst" -> FieldGet(args.[0], "_1") :> Statement<_>, tContext
        | "snd" -> FieldGet(args.[0], "_2") :> Statement<_>, tContext
        | "first" -> FieldGet(args.[0], "_1") :> Statement<_>, tContext
        | "second" -> FieldGet(args.[0], "_2") :> Statement<_>, tContext
        | "third" -> FieldGet(args.[0], "_3") :> Statement<_>, tContext
        | c -> failwithf "Unsupported call: %s" c

    and private itemHelper exprs hostVar tContext =
        let (idx, tContext) =
            match exprs with
            | hd :: _ -> translateAsExpr hd tContext
            | [] -> failwith "Array index missed!"
        let (hVar, _) = hostVar

        idx, tContext, hVar

    and private translateSpecificPropGet expr propName exprs targetContext =
        // TODO: Refactoring: Safe pattern matching by expr type.

        let hostVar = translateAsExpr expr targetContext
        match propName with
        | "globalid0i"
        | "globalid0" ->
            FunCall("get_global_id", [Const(PrimitiveType Int, "0")]) :> Expression<_>, targetContext
        | "globalid1i"
        | "globalid1" ->
            FunCall("get_global_id", [Const(PrimitiveType Int, "1")]) :> Expression<_>, targetContext
        | "globalid2i"
        | "globalid2" ->
            FunCall("get_global_id", [Const(PrimitiveType Int, "2")]) :> Expression<_>, targetContext
        | "localid0" ->
            FunCall("get_local_id", [Const(PrimitiveType Int, "0")]) :> Expression<_>, targetContext
        | "localid1" ->
            FunCall("get_local_id", [Const(PrimitiveType Int, "1")]) :> Expression<_>, targetContext
        | "localid2" ->
            FunCall("get_local_id", [Const(PrimitiveType Int, "2")]) :> Expression<_>, targetContext
        | "item" ->
            let (idx, tContext, hVar) = itemHelper exprs hostVar targetContext
            Item(hVar, idx) :> Expression<_>, tContext
        | _ -> failwithf "Unsupported property in kernel: %A" propName

    and private translatePropGet
        (exprOpt: Expr Option)
        (propInfo: PropertyInfo)
        exprs
        (targetContext: TargetContext<_, _>)
        =
        let propName = propInfo.Name.ToLowerInvariant()

        match exprOpt with
        | Some expr ->
            let exprType = expr.Type
            if targetContext.UserDefinedTypes.Contains exprType then
                let exprTypeName = expr.Type.Name.ToLowerInvariant()
                if targetContext.UserDefinedStructsOpenCLDeclaration.ContainsKey exprTypeName then
                    translateStructFieldGet expr propInfo.Name targetContext
                else
                    translateUnionFieldGet expr propInfo targetContext
            else
                translateSpecificPropGet expr propName exprs targetContext
        | None -> failwithf "Unsupported static property get in kernel: %A" propName

    and private translatePropSet exprOpt (propInfo: System.Reflection.PropertyInfo) exprs newVal targetContext =
        // Todo: Safe pattern matching (item) by expr type
        let propName = propInfo.Name.ToLowerInvariant()

        match exprOpt with
        | Some expr ->
            let hostVar = translateAsExpr expr targetContext
            let (newVal, tContext) =
                translateAsExpr
                    newVal
                    (match hostVar with (v, c) -> c)
            match propInfo.Name.ToLowerInvariant() with
            | "item" ->
                let idx, tContext, hVar = itemHelper exprs hostVar tContext
                let item = Item(hVar, idx)
                Assignment(Property(PropertyType.Item item), newVal) :> Statement<_>, tContext
            | _ ->
                let r, tContext = translateFieldSet expr propInfo.Name exprs.[0] targetContext
                r :> Statement<_>, tContext
        | None -> failwithf "Unsupported static property set in kernel: %A" propName

    and translateAsExpr expr (targetContext: TargetContext<_, _>) =
        let (r: Node<_>), tc = translate expr (targetContext: TargetContext<_, _>)
        (r :?> Expression<_>), tc

    and getVar (clVarName: string) (targetContext: TargetContext<_, _>) = Variable clVarName

    and translateVar (var: Var) (targetContext: TargetContext<_, _>) =
        //getVar var.Name targetContext
        let vName = targetContext.Namer.GetCLVarName var.Name
        match vName with
        | Some n -> getVar n targetContext
        | None ->
            sprintf "Seems, that you try to use variable with name %A, that declared out of quotation." var.Name
            + "Please, pass it as quoted function's parametaer."
            |> failwith

    and translateValue (value: obj) (sType: System.Type) targetContext =
        let mutable _type = None
        let v =
            let s = string value
            match sType.Name.ToLowerInvariant() with
            | "boolean" ->
                _type <- Type.translate sType false None targetContext |> Some
                if s.ToLowerInvariant() = "false" then "0" else "1"
            | t when t.EndsWith "[]" ->
                let arr =
                    match t with
                    | "int32[]" -> value :?> array<int> |> Array.map string
                    | "byte[]" -> value :?> array<byte> |> Array.map string
                    | "single[]" -> value :?> array<float32> |> Array.map string
                    | _ -> failwith "Unsupported array type."
                _type <- Type.translate sType false (Some arr.Length) targetContext |> Some
                arr |> String.concat ", " |> fun s -> "{ " + s + "}"
            | _ ->
                _type <- Type.translate sType false None targetContext |> Some
                s
        Const(_type.Value, v)

    and translateVarSet (var: Var) (expr: Expr) targetContext =
        let var = translateVar var targetContext
        let expr, tContext = translateCond (*TranslateAsExpr*) expr targetContext
        Assignment(Property(PropertyType.Var var), expr), tContext

    and translateCond (cond: Expr) targetContext =
        match cond with
        | Patterns.IfThenElse (cond, _then, _else) ->
            let l, tContext = translateCond cond targetContext
            let r, tContext = translateCond _then tContext
            let e, tContext = translateCond _else tContext
            let asBit = tContext.TranslatorOptions.Contains(BoolAsBit)
            let o1 =
                match r with
                | :? Const<Lang> as c when c.Val = "1" -> l
                | _ -> Binop((if asBit then BitAnd else And), l, r) :> Expression<_>
            match e with
            | :? Const<Lang> as c when c.Val = "0" -> o1
            | _ -> Binop((if asBit then BitOr else Or), o1, e) :> Expression<_>
            , tContext
        | _ -> translateAsExpr cond targetContext

    and toStb (s: Node<_>) =
        match s with
        | :? StatementBlock<_> as s -> s
        | x -> StatementBlock <| ResizeArray [x :?> Statement<_>]

    and translateIf (cond: Expr) (thenBranch: Expr) (elseBranch: Expr) targetContext =
        let cond, tContext = translateCond cond targetContext
        let _then, tContext =
            let t, tc = translate thenBranch (clearContext targetContext)
            toStb t, tc
        let _else, tContext =
            match elseBranch with
            | Patterns.Value (null, sType) -> None, tContext
            | _ ->
                let r, tContext = translate elseBranch (clearContext targetContext)
                Some(toStb r), tContext
        IfThenElse(cond, _then, _else), targetContext

    and translateForIntegerRangeLoop
        (i: Var)
        (from: Expr)
        (_to: Expr)
        (_do: Expr)
        (targetContext: TargetContext<_, _>)
        =
        let iName = targetContext.Namer.LetStart i.Name
        let v = getVar iName targetContext
        let var = translateBinding i iName from targetContext
        let condExpr, tContext = translateAsExpr _to targetContext
        targetContext.Namer.LetIn i.Name
        let body, tContext = translate _do (clearContext targetContext)
        let cond = Binop(LessEQ, v, condExpr)
        let condModifier = Unop(UOp.Incr, v)
        targetContext.Namer.LetOut()
        ForIntegerLoop(var, cond, condModifier, toStb body), targetContext

    and translateWhileLoop condExpr bodyExpr targetContext =
        let nCond, tContext = translateCond condExpr targetContext
        let nBody, tContext = translate bodyExpr tContext
        WhileLoop(nCond, toStb nBody), tContext

    and translateSeq expr1 expr2 (targetContext: TargetContext<_, _>) =
        let linearized = ResizeArray<_>()
        let rec go e =
            match e with
            | Patterns.Sequential (e1, e2) ->
                go e1
                go e2
            | e -> linearized.Add e
        go expr1
        go expr2
        let decls = ResizeArray(targetContext.VarDecls)
        targetContext.VarDecls.Clear()
        let tContext =
            linearized
            |> ResizeArray.fold
                (fun (context: TargetContext<_, _>) s ->
                    context.VarDecls.Clear()
                    let nExpr, tContext = translate s targetContext
                    match nExpr: Node<_> with
                    | :? StatementBlock<Lang> as s1 -> decls.AddRange(s1.Statements)
                    | s1 -> decls.Add(s1 :?> Statement<_>)
                    tContext
                ) targetContext

        let stmt = StatementBlock decls
        stmt, tContext

    and translateApplication expr1 expr2 targetContext =
        let rec go expr _vals args =
            match expr with
            | Patterns.Lambda (v, e) -> go e _vals (v :: args)
            | Patterns.Application (e1, e2) -> go e1 (e2 :: _vals) args
            | e ->
                if _vals.Length = args.Length then
                    let d = List.zip (List.rev args) _vals |> dict

                    //failwith "Partial evaluation is not supported in kernel function."
                    e.Substitute(fun v -> if d.ContainsKey v then Some d.[v] else None), true
                else
                    e, false
        let body, doing = go expr1 [expr2] []
        body, doing, targetContext
    //if(body = null) then
    //    translateApplicationFun expr1 expr2 targetContext
    //else

    //else
    //let getStatementFun = dictionaryFun.[expr.
    //FunCall(expr.ToString(), _vals) :> Statement<_>,targetContext
    //failwith "-Partial evaluation is not supported in kernel function."

    and translateApplicationFun expr1 expr2 targetContext =
        let rec go expr _vals args =
            match expr with
            | Patterns.Lambda (v, e) -> go e _vals (v :: args)
            | Patterns.Application (e1, e2) ->
                let exp, tc = (translateAsExpr(e2) targetContext)
                go e1 (exp :: _vals) args
            | e ->
                let listArg = List.rev _vals
                let funName =
                    match expr with
                    | Patterns.ValueWithName (_, _, name) -> name
                    | _ -> expr.ToString()
                let funCall = FunCall(funName, _vals) :> Statement<_>
                funCall, targetContext
        //failwith "-Partial evaluation is not supported in kernel function."
        let exp, tc = translateAsExpr expr2 targetContext
        go expr1 [exp] []

    and translateFieldSet host name _val context =
        let hostE, tc = translateAsExpr host context
        let field = name //fldInfo.Name
        let valE, tc = translateAsExpr _val tc
        let res = FieldSet(hostE, field, valE)
        res, tc

    and translateStructFieldGet host name context =
        let hostE, tc = translateAsExpr host context
        let field = name //fldInfo.Name
        let res = FieldGet(hostE, field) :> Expression<_>
        res, tc

    and translateUnionFieldGet expr (propInfo: PropertyInfo) targetContext =
        let exprTypeName = expr.Type.Name.ToLowerInvariant()
        let unionType = targetContext.UserDefinedUnionsOpenCLDeclaration.[exprTypeName]

        let unionValueExpr, targetContext = translateAsExpr expr targetContext

        let caseName = propInfo.DeclaringType.Name
        let unionCaseField = unionType.GetCaseByName caseName

        match unionCaseField with
        | None ->
            failwithf
                "Union field get translation error:
                             union %A doesn't have case %A"
                unionType.Name
                caseName
        | Some unionCaseField ->
            let r =
                FieldGet<_>(
                    FieldGet<_>(FieldGet<_>(unionValueExpr, unionType.Data.Name), unionCaseField.Name),
                    propInfo.Name
                )
                :> Expression<_>
            r, targetContext

    and translate expr (targetContext: TargetContext<_, _>) =
        match expr with
        | Patterns.AddressOf expr -> "AdressOf is not suported:" + string expr |> failwith
        | Patterns.AddressSet expr -> "AdressSet is not suported:" + string expr |> failwith
        | Patterns.Application (expr1, expr2) ->
            let (e, appling, targetContext) = translateApplication expr1 expr2 targetContext
            if appling then
                translate e targetContext
            else
                let (r, tContext) = translateApplicationFun expr1 expr2 targetContext
                r :> Node<_>, tContext
        | DerivedPatterns.SpecificCall <@@ print @@> (_, _, args) ->
            match args with
            | [ Patterns.ValueWithName (argTypes, _, _);
                Patterns.ValueWithName (formatStr, _, _);
                Patterns.ValueWithName (argValues, _, _) ] ->
                let formatStrArg = Const(PrimitiveType ConstStringLiteral, formatStr :?> string) :> Expression<_>
                let args', targetContext' = translateListOfArgs (argValues :?> list<Expr>) targetContext
                FunCall("printf", formatStrArg :: args') :> Node<_>, targetContext'
            | _ -> failwith "printf: something going wrong."

        | DerivedPatterns.SpecificCall <@ atomic @> (_, _, [func]) ->
            targetContext.Flags.enableAtomic <- true
            FunCall("atom_add", [Ptr <| Variable("x"); Variable("y")]) :> Node<_>, targetContext
            // match func with
            // // list of tupled params; (+) -> [[x];[y]], ...
            // | DerivedPatterns.Lambdas (args, body) ->
            //     // тут нужно параметры лямбды в контекст вносить
            //     // let ([x; y], tContext) = translateListOfArgs args targetContext
            //     match body with
            //     | DerivedPatterns.SpecificCall <@ add @> (_, opType :: _, [x; y]) ->
            //         let ([x; y], tContext) = translateListOfArgs [x; y] targetContext
            //         match opType with
            //         // как мы тут определяем доступно ли расширение на доп атомики или нет??
            //         | t when t = typeof<int> || t = typeof<uint32> ->
            //             FunCall("atom_add", [Ptr x; y]) :> Node<_>, tContext
            //         | _ -> failwith "atomic: incorrect type"
            //     | DerivedPatterns.SpecificCall <@ sub @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ inc @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ dec @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ xchg @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ min @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ max @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ and' @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ or' @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | DerivedPatterns.SpecificCall <@ xor @> (_, types, [x; y]) -> failwith "Not implemented"
            //     | _ -> failwith "atomic: something going wrong."
            // | _ -> failwith "atomic: arg should be lambda"

        | Patterns.Call (exprOpt, mInfo, args) ->
            let r, tContext = translateCall exprOpt mInfo args targetContext
            r :> Node<_>, tContext
        | Patterns.Coerce (expr, sType) -> "Coerce is not suported:" + string expr |> failwith
        | Patterns.DefaultValue sType -> "DefaulValue is not suported:" + string expr |> failwith
        | Patterns.FieldGet (exprOpt, fldInfo) ->
            match exprOpt with
            | Some expr ->
                let r, tContext = translateStructFieldGet expr fldInfo.Name targetContext
                r :> Node<_>, tContext
            | None -> failwithf "FieldGet for empty host is not suported. Field: %A" fldInfo.Name
        | Patterns.FieldSet (exprOpt, fldInfo, expr) ->
            match exprOpt with
            | Some e ->
                let r, tContext = translateFieldSet e fldInfo.Name expr targetContext
                r :> Node<_>, tContext
            | None -> failwithf "Fileld set with empty host is not supported. Field: %A" fldInfo
        | Patterns.ForIntegerRangeLoop (i, from, _to, _do) ->
            let r, tContext = translateForIntegerRangeLoop i from _to _do targetContext
            r :> Node<_>, tContext
        | Patterns.IfThenElse (cond, thenExpr, elseExpr) ->
            let r, tContext = translateIf cond thenExpr elseExpr targetContext
            r :> Node<_>, tContext
        | Patterns.Lambda (var, _expr) ->
            // translateLambda var expr targetContext
            failwithf "Lambda is not suported: %A" expr
        | Patterns.Let (var, expr, inExpr) ->
            match var.Name with
            | "___providedCallInfo" -> translateProvidedCall expr targetContext
            | _ -> translateLet var expr inExpr targetContext

        | Patterns.LetRecursive (bindings, expr) -> "LetRecursive is not suported:" + string expr |> failwith
        | Patterns.NewArray (sType, exprs) -> "NewArray is not suported:" + string expr |> failwith
        | Patterns.NewDelegate (sType, vars, expr) -> "NewDelegate is not suported:" + string expr |> failwith
        | Patterns.NewObject (constrInfo, exprs) ->
            let p = constrInfo.GetParameters()
            let p2 = constrInfo.GetMethodBody()
            if targetContext.UserDefinedTypes.Contains(constrInfo.DeclaringType) then
                let structInfo =
                    targetContext.UserDefinedStructsOpenCLDeclaration.[constrInfo.DeclaringType.Name.ToLowerInvariant()]
                let cArgs = exprs |> List.map (fun x -> translateAsExpr x targetContext)
                let res = NewStruct<_>(structInfo, cArgs |> List.unzip |> fst)
                res :> Node<_>, targetContext
            else
                "NewObject is not suported:" + string expr |> failwith
        | Patterns.NewRecord (sType, exprs) -> "NewRecord is not suported:" + string expr |> failwith
        | Patterns.NewTuple (exprs) ->
            let mutable n = 0
            let baseTypes = [| for i in 0 .. exprs.Length - 1 -> exprs.[i].Type |]
            let elements =
                [
                    for i in 0 .. exprs.Length - 1 ->
                        {
                            Name = "_" + (i + 1).ToString()
                            Type = Type.translate baseTypes.[i] false None targetContext
                        }
                ]
            let mutable s = ""
            for i in 0 .. baseTypes.Length - 1 do
                s <- s + baseTypes.[i].Name
            if not (targetContext.TupleDecls.ContainsKey(s)) then
                targetContext.TupleNumber <- targetContext.TupleNumber + 1
                targetContext.TupleDecls.Add(s, targetContext.TupleNumber)
                let a = StructType<Lang>("tuple" + targetContext.TupleNumber.ToString(), elements)
                targetContext.TupleList.Add(a)
                let cArgs = exprs |> List.map (fun x -> translateAsExpr x targetContext)
                NewStruct<_>(a, cArgs |> List.unzip |> fst) :> Node<_>, targetContext
            else
                let a =
                    StructType<Lang>("tuple" + (targetContext.TupleDecls.Item(s)).ToString(), elements)
                let cArgs = exprs |> List.map (fun x -> translateAsExpr x targetContext)
                NewStruct<_>(a, cArgs |> List.unzip |> fst) :> Node<_>, targetContext
        | Patterns.NewUnionCase (unionCaseInfo, exprs) ->
            let unionType = unionCaseInfo.DeclaringType
            if not <| targetContext.UserDefinedTypes.Contains(unionType) then
                failwithf "Union type %s is not registered" unionType.Name

            let typeName = unionType.Name.ToLowerInvariant()
            let unionInfo = targetContext.UserDefinedUnionsOpenCLDeclaration.[typeName]


            let tag = Const(unionInfo.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>
            let args =
                match unionInfo.GetCaseByTag unionCaseInfo.Tag with
                | None -> []
                | Some field ->
                    let structArgs = exprs |> List.map (fun x -> fst <| translateAsExpr x targetContext)
                    let data =
                        NewUnion(
                            unionInfo.Data.Type :?> UnionClInplaceType<_>,
                            field.Name,
                            NewStruct(field.Type :?> StructType<_>, structArgs)
                        )
                    [ data :> Expression<_> ]

            NewStruct(unionInfo, tag :: args) :> Node<_>, targetContext
        | Patterns.PropertyGet (exprOpt, propInfo, exprs) ->
            let res, tContext = translatePropGet exprOpt propInfo exprs targetContext
            (res :> Node<_>), tContext
        | Patterns.PropertySet (exprOpt, propInfo, exprs, expr) ->
            let res, tContext = translatePropSet exprOpt propInfo exprs expr targetContext
            res :> Node<_>, tContext
        | Patterns.Sequential (expr1, expr2) ->
            let res, tContext = translateSeq expr1 expr2 targetContext
            res :> Node<_>, tContext
        | Patterns.TryFinally (tryExpr, finallyExpr) -> "TryFinally is not suported:" + string expr |> failwith
        | Patterns.TryWith (expr1, var1, expr2, var2, expr3) -> "TryWith is not suported:" + string expr |> failwith
        | Patterns.TupleGet (expr, i) ->
            let r, tContext = translateStructFieldGet expr ("_" + (string (i + 1))) targetContext
            r :> Node<_>, tContext
        | Patterns.TypeTest (expr, sType) -> "TypeTest is not suported:" + string expr |> failwith
        | Patterns.UnionCaseTest (expr, unionCaseInfo) ->
            let unionTypeName = expr.Type.Name.ToLowerInvariant()
            let unionDecl = targetContext.UserDefinedUnionsOpenCLDeclaration.[unionTypeName]

            let unionVarExpr, tc = translateAsExpr expr targetContext
            let unionGetTagExpr = FieldGet(unionVarExpr, unionDecl.Tag.Name) :> Expression<_>
            let tagExpr = Const(unionDecl.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>

            Binop(BOp.EQ, unionGetTagExpr, tagExpr) :> Node<_>, tc
        | Patterns.ValueWithName (_obj, sType, name) ->
            // Here is the only use of TargetContext.InLocal
            if sType.ToString().EndsWith "[]" && not targetContext.InLocal then
                targetContext.Namer.AddVar name
                let res = translateValue _obj sType targetContext
                targetContext.TopLevelVarsDeclarations.Add(
                    VarDecl(res.Type, name, Some(res :> Expression<_>), AddressSpaceQualifier.Constant)
                )
                let var = Var(name, sType)
                translateVar var targetContext :> Node<_>, targetContext
            else
                translateValue _obj sType targetContext :> Node<_>, targetContext
        | Patterns.Value (_obj, sType) -> translateValue _obj sType targetContext :> Node<_>, targetContext
        | Patterns.Var var -> translateVar var targetContext :> Node<_>, targetContext
        | Patterns.VarSet (var, expr) ->
            let res, tContext = translateVarSet var expr targetContext
            res :> Node<_>, tContext
        | Patterns.WhileLoop (condExpr, bodyExpr) ->
            let r, tContext = translateWhileLoop condExpr bodyExpr targetContext
            r :> Node<_>, tContext
        | other -> "OTHER!!! :" + string other |> failwith

    and private translateLet var expr inExpr targetContext =
        let bName = targetContext.Namer.LetStart var.Name

        let vDecl =
            match expr with
            | DerivedPatterns.SpecificCall <@@ local @@> (_, _, _) ->
                let vType = Type.translate var.Type false None targetContext
                VarDecl(vType, bName, None, spaceModifier = Local)
            | DerivedPatterns.SpecificCall <@@ localArray @@> (_, _, [arg]) ->
                let (expr, newTargetContext) = translateCond arg targetContext
                let arrayLength =
                    match expr with
                    | :? Const<Lang> as c -> Some <| int c.Val
                    | other -> failwithf "Calling localArray with a non-const argument %A" other
                let arrayType = Type.translate var.Type false arrayLength newTargetContext
                VarDecl(arrayType, bName, None, spaceModifier = Local)
            | _ -> translateBinding var bName expr targetContext

        targetContext.VarDecls.Add vDecl
        targetContext.Namer.LetIn var.Name

        let res, tContext = clearContext targetContext |> translate inExpr //вот тут мб нужно проверять на call или application
        let sb = ResizeArray(targetContext.VarDecls |> Seq.cast<Statement<_>>)
        targetContext.TupleDecls.Clear()
        targetContext.TupleList.Clear()
        for td in tContext.TupleDecls do
            targetContext.TupleDecls.Add(td.Key, td.Value)
        for t in tContext.TupleList do
            targetContext.TupleList.Add(t)
        targetContext.TupleNumber <- tContext.TupleNumber
        match res with
        | :? StatementBlock<Lang> as s -> sb.AddRange s.Statements
        | _ -> sb.Add(res :?> Statement<_>)

        targetContext.Namer.LetOut()
        StatementBlock<_>(sb) :> Node<_>, (clearContext targetContext)

    and private translateProvidedCall expr (targetContext: TargetContext<_, _>) =
        let rec traverse expr args =
            match expr with
            | Patterns.Value (calledName, sType) ->
                match sType.Name.ToLowerInvariant() with
                | "string" -> (calledName :?> string), args
                | _ ->
                    "Failed to parse provided call, expected string call name: " + string expr
                    |> failwith
            | Patterns.Sequential (expr1, expr2) ->
                let updatedArgs =
                    match expr2 with
                    | Patterns.Value (null, _) -> args // the last item in the sequence is null
                    | _ -> (translateAsExpr(expr2) targetContext |> fst) :: args
                traverse expr1 updatedArgs
            | _ -> "Failed to parse provided call: " + string expr |> failwith
        let funCall = FunCall(traverse expr []) :> Node<_>
        funCall, targetContext
