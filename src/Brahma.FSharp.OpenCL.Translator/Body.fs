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
    // TODO is it really clear context?
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

    let rec private translateBinding (var: Var) newName (expr: Expr) =
        translator {
            let! body = translateCond (*TranslateAsExpr*) expr
            let vType =
                match (body: Expression<_>) with
                | :? Const<_> as c ->
                    c.Type
                | :? ArrayInitializer<_> as ai ->
                    Type.translate var.Type false (Some ai.Length)
                | _ -> Type.translate var.Type false None

            return VarDecl(vType, newName, Some body)
        }

    and private translateListOfArgs (args: Expr list) =
        args
        |> List.fold
            (fun res arg ->
                translator {
                    let! res = res
                    let! r = translateCond arg
                    return r :: res
                }
            ) (Translator.return' [])
        |> fun args -> TranslationContext.map List.rev args

    and private translateCall exprOpt (mInfo: System.Reflection.MethodInfo) _args =
        translator {
            let! args = translateListOfArgs _args

            match mInfo.Name.ToLowerInvariant() with
            | "op_multiply" -> return Binop(Mult, args.[0], args.[1]) :> Statement<_>
            | "op_addition" -> return Binop(Plus, args.[0], args.[1]) :> Statement<_>
            | "op_division" -> return Binop(Div, args.[0], args.[1]) :> Statement<_>
            | "op_lessthan" -> return Binop(Less, args.[0], args.[1]) :> Statement<_>
            | "op_lessthanorequal" -> return Binop(LessEQ, args.[0], args.[1]) :> Statement<_>
            | "op_greaterthan" -> return Binop(Great, args.[0], args.[1]) :> Statement<_>
            | "op_greaterthanorequal" -> return Binop(GreatEQ, args.[0], args.[1]) :> Statement<_>
            | "op_equality" -> return Binop(EQ, args.[0], args.[1]) :> Statement<_>
            | "op_inequality" -> return Binop(NEQ, args.[0], args.[1]) :> Statement<_>
            | "op_subtraction" -> return Binop(Minus, args.[0], args.[1]) :> Statement<_>
            | "op_unarynegation" -> return Unop(UOp.Minus, args.[0]) :> Statement<_>
            | "op_modulus" -> return Binop(Remainder, args.[0], args.[1]) :> Statement<_>
            | "op_bitwiseand" -> return Binop(BitAnd, args.[0], args.[1]) :> Statement<_>
            | "op_bitwiseor" -> return Binop(BitOr, args.[0], args.[1]) :> Statement<_>
            | "op_leftshift" -> return Binop(LeftShift, args.[0], args.[1]) :> Statement<_>
            | "op_rightshift" -> return Binop(RightShift, args.[0], args.[1]) :> Statement<_>
            | "op_booleanand" -> return Binop(And, args.[0], args.[1]) :> Statement<_>
            | "op_booleanor" -> return Binop(Or, args.[0], args.[1]) :> Statement<_>
            | "op_lessbangplusgreater"
            | "op_lessbangplus"
            | "atomicadd" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_add", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "op_lessbangmunus"
            | "op_lessbangmunusgreater"
            | "atomicsub" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_sub", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "op_lessbanggreater"
            | "op_lessbang"
            | "atomicxchg" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_xchg", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "amax"
            | "amaxr"
            | "atomicmax" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_max", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "amin"
            | "aminr"
            | "atomicmin" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_min", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "aincr"
            | "aincrr"
            | "atomicinc" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_inc", [Ptr args.[0]]) :> Statement<_>
            | "adecr"
            | "adecrr"
            | "atomicdec" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_dec", [Ptr args.[0]]) :> Statement<_>
            | "acompexch"
            | "acompexchr"
            | "atomiccmpxchg" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_cmpxchg", [Ptr args.[0]; args.[1]; args.[2]]) :> Statement<_>
            | "atomicand" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_and", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "atomicor" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_or", [Ptr args.[0]; args.[1]]) :> Statement<_>
            | "atomicxor" ->
                do! TranslationContext.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_xor", [Ptr args.[0]; args.[1]]) :> Statement<_>

            | "todouble" -> return Cast(args.[0], PrimitiveType Float) :> Statement<_>
            | "toint" -> return Cast(args.[0], PrimitiveType Int) :> Statement<_>
            | "toint16" -> return Cast(args.[0], PrimitiveType Short) :> Statement<_>
            | "tosingle" -> return Cast(args.[0], PrimitiveType Float) :> Statement<_>
            | "tobyte" -> return Cast(args.[0], PrimitiveType UChar) :> Statement<_>
            | "touint32" -> return Cast(args.[0], PrimitiveType UInt) :> Statement<_>
            | "touint16" -> return Cast(args.[0], PrimitiveType UShort) :> Statement<_>
            | "toint64" -> return Cast(args.[0], PrimitiveType Long) :> Statement<_>
            | "touint64" -> return Cast(args.[0], PrimitiveType ULong) :> Statement<_>
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
                return
                    if
                        mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("System.Math") ||
                        mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
                    then
                        FunCall(fName, args) :> Statement<_>
                    else
                        failwithf
                            "Seems, that you use math function with name %s not from System.Math or Microsoft.FSharp.Core.Operators"
                            fName
            | "abs" as fName ->
                return
                    if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                        FunCall("fabs", args) :> Statement<_>
                    else
                        failwithf
                            "Seems, that you use math function with name %s not from System.Math or Microsoft.FSharp.Core.Operators"
                            fName
            | "powinteger" as fName ->
                return
                    if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                        FunCall("powr", args) :> Statement<_>
                    else
                        failwithf
                            "Seems, that you use math function with name %s not from System.Math or Microsoft.FSharp.Core.Operators"
                            fName
            | "ref" -> return Ptr args.[0] :> Statement<_>
            | "op_dereference" -> return IndirectionOp args.[0] :> Statement<_>
            | "op_colonequals" ->
                return Assignment(Property(PropertyType.VarReference(IndirectionOp args.[0])), args.[1]) :> Statement<_>
            | "setarray" ->
                let item = Item(args.[0], args.[1])
                return Assignment(Property(PropertyType.Item item), args.[2]) :> Statement<_>
            | "getarray" -> return Item(args.[0], args.[1]) :> Statement<_>
            | "not" -> return Unop(UOp.Not, args.[0]) :> Statement<_>
            | "_byte" -> return args.[0] :> Statement<_>
            | "barrier" -> return Barrier() :> Statement<_>
            | "local" -> return failwith "Calling the local function is allowed only at the top level of the let binding"
            | "arrayLocal" -> return failwith "Calling the localArray function is allowed only at the top level of the let binding"
            | "zerocreate" ->
                let length =
                    match args.[0] with
                    | :? Const<_> as c -> int c.Val
                    | other -> failwithf "Calling Array.zeroCreate with a non-const argument: %A" other

                return ZeroArray length :> Statement<_>
            | "fst" -> return FieldGet(args.[0], "_1") :> Statement<_>
            | "snd" -> return FieldGet(args.[0], "_2") :> Statement<_>
            | "first" -> return FieldGet(args.[0], "_1") :> Statement<_>
            | "second" -> return FieldGet(args.[0], "_2") :> Statement<_>
            | "third" -> return FieldGet(args.[0], "_3") :> Statement<_>
            | other -> return failwithf "Unsupported call: %s" other
        }

    and private itemHelper exprs hostVar =
        translator {
            let! idx = translator {
                match exprs with
                | hd :: _ -> return translateAsExpr hd
                | [] -> return failwith "Array index missed!"
            }

            let (hVar, _) = hostVar

            return idx, hVar
        }


    and private translateSpecificPropGet expr propName exprs targetContext =
        translator {
            // TODO: Refactoring: Safe pattern matching by expr type.

            let hostVar = translateAsExpr expr targetContext
            match propName with
            | "globalid0i"
            | "globalid0" ->
                return FunCall("get_global_id", [Const(PrimitiveType Int, "0")]) :> Expression<_>
            | "globalid1i"
            | "globalid1" ->
                return FunCall("get_global_id", [Const(PrimitiveType Int, "1")]) :> Expression<_>
            | "globalid2i"
            | "globalid2" ->
                return FunCall("get_global_id", [Const(PrimitiveType Int, "2")]) :> Expression<_>
            | "localid0" ->
                return FunCall("get_local_id", [Const(PrimitiveType Int, "0")]) :> Expression<_>
            | "localid1" ->
                return FunCall("get_local_id", [Const(PrimitiveType Int, "1")]) :> Expression<_>
            | "localid2" ->
                return FunCall("get_local_id", [Const(PrimitiveType Int, "2")]) :> Expression<_>
            | "item" ->
                let! (idx, hVar) = itemHelper exprs hostVar
                return Item(hVar, idx) :> Expression<_>
            | _ -> return failwithf "Unsupported property in kernel: %A" propName
        }

    and private translatePropGet (exprOpt: Expr Option) (propInfo: PropertyInfo) exprs =
        translator {
            let propName = propInfo.Name.ToLowerInvariant()

            match exprOpt with
            | Some expr ->
                let exprType = expr.Type
                let! b = TranslationContext.gets (fun context -> context.UserDefinedTypes.Contains exprType)
                if b then
                    let exprTypeName = expr.Type.Name.ToLowerInvariant()
                    let! b = TranslationContext.gets (fun context ->  context.UserDefinedStructsOpenCLDeclaration.ContainsKey exprTypeName)
                    if b then
                        return! translateStructFieldGet expr propInfo.Name
                    else
                        return! translateUnionFieldGet expr propInfo
                else
                    return! translateSpecificPropGet expr propName exprs
            | None -> return failwithf "Unsupported static property get in kernel: %A" propName
        }


    and private translatePropSet exprOpt (propInfo: System.Reflection.PropertyInfo) exprs newVal =
        translator {
            // Todo: Safe pattern matching (item) by expr type
            let propName = propInfo.Name.ToLowerInvariant()

            match exprOpt with
            | Some expr ->
                let! hostVar = translateAsExpr expr
                let! newVal = translateAsExpr newVal

                return
                    match propInfo.Name.ToLowerInvariant() with
                    | "item" ->
                        let idx, hVar = itemHelper exprs hostVar
                        let item = Item(hVar, idx)
                        Assignment(Property(PropertyType.Item item), newVal) :> Statement<_>
                    | _ ->
                        let r = translateFieldSet expr propInfo.Name exprs.[0]
                        r :> Statement<_>
            | None -> return failwithf "Unsupported static property set in kernel: %A" propName
        }

    and translateAsExpr expr =
        translator {
            let! (r: Node<_>) = translate expr
            return (r :?> Expression<_>)
        }

    and getVar (clVarName: string) =
        translator {
            return Variable clVarName
        }

    and translateVar (var: Var) =
        translator {
            //getVar var.Name targetContext
            match! TranslationContext.gets (fun context -> context.Namer.GetCLVarName var.Name) with
            | Some n -> return getVar n
            | None ->
                return
                    failwithf
                        "Seems, that you try to use variable with name %A, that declared out of quotation.
                        Please, pass it as quoted function's parametaer." var.Name
        }

    and translateValue (value: obj) (sType: System.Type) =
        translator {
            let mutable _type = None
            let v =
                let s = string value
                match sType.Name.ToLowerInvariant() with
                | "boolean" ->
                    _type <- Type.translate sType false None |> Some
                    if s.ToLowerInvariant() = "false" then "0" else "1"
                | t when t.EndsWith "[]" ->
                    let arr =
                        match t with
                        | "int32[]" -> value :?> array<int> |> Array.map string
                        | "byte[]" -> value :?> array<byte> |> Array.map string
                        | "single[]" -> value :?> array<float32> |> Array.map string
                        | _ -> failwith "Unsupported array type."
                    _type <- Type.translate sType false (Some arr.Length) |> Some
                    arr
                    |> String.concat ", "
                    |> fun s -> "{ " + s + "}"
                | _ ->
                    _type <- Type.translate sType false None |> Some
                    s
            return Const(_type.Value, v)
        }

    and translateVarSet (var: Var) (expr: Expr) =
        translator {
            let! var = translateVar var
            let! expr = translateCond (*TranslateAsExpr*) expr
            return Assignment(Property(PropertyType.Var var), expr)
        }

    and translateCond (cond: Expr) : TranslationContext<Expression<_>> =
        translator {
            match cond with
            | Patterns.IfThenElse (cond, _then, _else) ->
                let! l = translateCond cond
                let! r = translateCond _then
                let! e = translateCond _else
                let! asBit = TranslationContext.gets <| fun context -> context.TranslatorOptions.Contains BoolAsBit
                let o1 =
                    match r with
                    | :? Const<Lang> as c when c.Val = "1" -> l
                    | _ -> Binop((if asBit then BitAnd else And), l, r) :> Expression<_>

                return
                    match e with
                    | :? Const<Lang> as c when c.Val = "0" -> o1
                    | _ -> Binop((if asBit then BitOr else Or), o1, e) :> Expression<_>
            | _ ->
                return! translateAsExpr cond
        }


    and toStb (s: Node<_>) =
        translator {
            return
                match s with
                | :? StatementBlock<_> as s -> s
                | x -> StatementBlock <| ResizeArray [x :?> Statement<_>]
        }

    // TODO wtf
    // and translateIf (cond: Expr) (thenBranch: Expr) (elseBranch: Expr) targetContext =
    //     let cond, tContext = translateCond cond targetContext
    //     let _then, tContext =
    //         let t, tc = translate thenBranch (clearContext targetContext)
    //         toStb t, tc
    //     let _else, tContext =
    //         match elseBranch with
    //         | Patterns.Value (null, sType) -> None, tContext
    //         | _ ->
    //             let r, tContext = translate elseBranch (clearContext targetContext)
    //             Some(toStb r), tContext
    //     IfThenElse(cond, _then, _else), targetContext
    and translateIf (cond: Expr) (thenBranch: Expr) (elseBranch: Expr) =
        translator {
            let! cond = translateCond cond
            let! _then = translate thenBranch >>= toStb
            let! _else = translator {
                match elseBranch with
                | Patterns.Value (null, sType) -> return None
                | _ ->
                    let! r = translate elseBranch >>= toStb
                    return Some r
            }

            return IfThenElse(cond, _then, _else)
        }

    // and translateForIntegerRangeLoop
    //     (i: Var)
    //     (from: Expr)
    //     (_to: Expr)
    //     (_do: Expr)
    //     (targetContext: TargetContext<_, _>)
    //     =
    //     let iName = targetContext.Namer.LetStart i.Name
    //     let v = getVar iName targetContext
    //     let var = translateBinding i iName from targetContext
    //     let condExpr, tContext = translateAsExpr _to targetContext
    //     targetContext.Namer.LetIn i.Name
    //     let body, tContext = translate _do (clearContext targetContext)
    //     let cond = Binop(LessEQ, v, condExpr)
    //     let condModifier = Unop(UOp.Incr, v)
    //     targetContext.Namer.LetOut()
    //     ForIntegerLoop(var, cond, condModifier, toStb body), targetContext
    and translateForIntegerRangeLoop
        (i: Var)
        (from: Expr)
        (_to: Expr)
        (_do: Expr) =

        translator {
            let! iName = TranslationContext.gets (fun context -> context.Namer.LetStart i.Name)
            let! v = getVar iName
            let! var = translateBinding i iName from
            let! condExpr = translateAsExpr _to
            do! TranslationContext.modify (fun context -> context.Namer.LetIn i.Name; context)
            let! body = translate _do >>= toStb
            let cond = Binop(LessEQ, v, condExpr)
            let condModifier = Unop(UOp.Incr, v)
            do! TranslationContext.modify (fun context -> context.Namer.LetOut(); context)
            return ForIntegerLoop(var, cond, condModifier, body)
        }

    and translateWhileLoop condExpr bodyExpr =
        translator {
            let! nCond = translateCond condExpr
            let! nBody = translate bodyExpr >>= toStb
            return WhileLoop(nCond, nBody)
        }

    and translateSeq expr1 expr2 =
        translator {
            let linearized = ResizeArray()
            let rec go e =
                match e with
                | Patterns.Sequential (e1, e2) ->
                    go e1
                    go e2
                | e -> linearized.Add e
            go expr1
            go expr2
            let! d = TranslationContext.gets (fun context -> context.VarDecls)
            let decls = ResizeArray d
            do! TranslationContext.modify (fun context -> context.VarDecls.Clear(); context)
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
        }

    and translateApplication expr1 expr2 targetContext =
        let rec go expr vals args =
            match expr with
            | Patterns.Lambda (v, e) -> go e vals (v :: args)
            | Patterns.Application (e1, e2) -> go e1 (e2 :: vals) args
            | expr ->
                if vals.Length = args.Length then
                    let d =
                        vals
                        |> List.zip (List.rev args)
                        |> dict

                    //failwith "Partial evaluation is not supported in kernel function."
                    expr.Substitute(fun v -> if d.ContainsKey v then Some d.[v] else None), true
                else
                    expr, false

        let (body, doing) = go expr1 [expr2] []
        body, doing, targetContext

    and translateApplicationFun expr1 expr2 targetContext =
        let rec go expr vals =
            match expr with
            // | Patterns.Lambda (v, e) -> go e vals (v :: args)
            | Patterns.Application (e1, e2) ->
                let (exp, tc) = translateAsExpr e2 targetContext
                go e1 (exp :: vals)
            | _ ->
                // TODO fix it
                // NOTE не поддерживается частичное применение
                // NOTE не поддерживается композиция функций (или функции высшего порядка)
                let funName =
                    match expr with
                    | Patterns.ValueWithName (_, _, name) -> name
                    | _ -> expr.ToString()

                FunCall(funName, vals) :> Statement<_>, targetContext

        let (exp, tc) = translateAsExpr expr2 targetContext
        go expr1 [exp]

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
                FieldGet(
                    FieldGet(
                        FieldGet(unionValueExpr, unionType.Data.Name),
                        unionCaseField.Name
                    ),
                    propInfo.Name
                )
                :> Expression<_>
            r, targetContext

    and translate expr =
        translator {
            match expr with
            | Patterns.AddressOf expr -> return failwithf "AdressOf is not suported: %O" expr
            | Patterns.AddressSet expr -> return failwithf "AdressSet is not suported: %O" expr

            | Patterns.Application (expr1, expr2) ->
                let! (e, appling) = translateApplication expr1 expr2
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
                    return FunCall("printf", formatStrArg :: args') :> Node<_>,
                | _ -> failwith "printf: something going wrong."

            // | DerivedPatterns.SpecificCall <@ atomic @> (_, _, [func]) ->
            //     targetContext.Flags.enableAtomic <- true
            //     FunCall("atom_add", [Ptr <| Variable("x"); Variable("y")]) :> Node<_>, targetContext
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
        }


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
