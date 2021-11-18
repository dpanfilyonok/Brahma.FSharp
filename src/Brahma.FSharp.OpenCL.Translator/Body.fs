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
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Brahma.FSharp.OpenCL

#nowarn "64"

module rec Body =
    // new var scope
    let private clearContext (targetContext: TranslationContext<'a, 'b>) =
        { targetContext with VarDecls = ResizeArray() }

    let private translateBinding (var: Var) newName (expr: Expr) =
        translation {
            let! body = translateCond (*TranslateAsExpr*) expr
            let! varType = translation {
                match (body: Expression<_>) with
                | :? Const<_> as c ->
                    return c.Type
                | :? ArrayInitializer<_> as ai ->
                    return! Type.translate var.Type |> State.using (fun ctx -> { ctx with AKind = ArrayArray ai.Length })
                | _ -> return! Type.translate var.Type
            }

            return VarDecl(varType, newName, Some body)
        }

    let private translateListOfArgs (args: Expr list) =
        args
        |> List.fold
            (fun state arg ->
                translation {
                    let! state = state
                    let! translated = translateCond arg
                    return translated :: state
                }
            ) (State.return' [])
        |> fun args -> State.map List.rev args

    let private translateCall exprOpt (mInfo: System.Reflection.MethodInfo) args =
        translation {
            let! args = translateListOfArgs args

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
            | "op_booleanand" ->
                let! flag = State.gets (fun context -> context.TranslatorOptions |> List.contains UseNativeBooleanType)
                if flag then
                    return Binop(And, args.[0], args.[1]) :> Statement<_>
                else
                    return Binop(BitAnd, args.[0], args.[1]) :> Statement<_>
            | "op_booleanor" ->
                let! flag = State.gets (fun context -> context.TranslatorOptions |> List.contains UseNativeBooleanType)
                if flag then
                    return Binop(Or, args.[0], args.[1]) :> Statement<_>
                else
                    return Binop(BitOr, args.[0], args.[1]) :> Statement<_>
            | "atomicadd" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_add", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicsub" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_sub", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicxchg" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_xchg", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicmax" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_max", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicmin" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_min", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicinc" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_inc", [args.[0]]) :> Statement<_>
            | "atomicdec" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_dec", [args.[0]]) :> Statement<_>
            | "atomiccmpxchg" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_cmpxchg", [args.[0]; args.[1]; args.[2]]) :> Statement<_>
            | "atomicand" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_and", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicor" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_or", [args.[0]; args.[1]]) :> Statement<_>
            | "atomicxor" ->
                do! State.modify (fun context -> context.Flags.enableAtomic <- true; context)
                return FunCall("atom_xor", [args.[0]; args.[1]]) :> Statement<_>
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
                if
                    mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("System.Math") ||
                    mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
                then
                    return FunCall(fName, args) :> Statement<_>
                else
                    return failwithf
                        "Seems, that you use math function with name %s not from System.Math or Microsoft.FSharp.Core.Operators"
                        fName
            | "abs" as fName ->
                if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                    return FunCall("fabs", args) :> Statement<_>
                else
                    return failwithf
                        "Seems, that you use math function with name %s not from System.Math or Microsoft.FSharp.Core.Operators"
                        fName
            | "powinteger" as fName ->
                if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                    return FunCall("powr", args) :> Statement<_>
                else
                    return failwithf
                        "Seems, that you use math function with name %s not from System.Math or Microsoft.FSharp.Core.Operators"
                        fName
            | "ref" -> return Ptr args.[0] :> Statement<_>
            | "op_dereference" -> return IndirectionOp args.[0] :> Statement<_>
            | "op_colonequals" ->
                return Assignment(Property(PropertyType.VarReference(IndirectionOp args.[0])), args.[1]) :> Statement<_>
            | "setarray" ->
                return Assignment(Property(PropertyType.Item(Item(args.[0], args.[1]))), args.[2]) :> Statement<_>
            | "getarray" -> return Item(args.[0], args.[1]) :> Statement<_>
            | "not" -> return Unop(UOp.Not, args.[0]) :> Statement<_>
            | "_byte" -> return args.[0] :> Statement<_>
            | "barrier" -> return Barrier() :> Statement<_>
            | "local" -> return failwith "Calling the local function is allowed only at the top level of the let binding"
            | "arrayLocal" -> return failwith "Calling the localArray function is allowed only at the top level of the let binding"
            | "zerocreate" ->
                let length =
                    match args.[0] with
                    | :? Const<Lang> as c -> int c.Val
                    | other -> failwithf "Calling Array.zeroCreate with a non-const argument: %A" other
                return ZeroArray length :> Statement<_>
            | "fst" -> return FieldGet(args.[0], "_1") :> Statement<_>
            | "snd" -> return FieldGet(args.[0], "_2") :> Statement<_>
            | "first" -> return FieldGet(args.[0], "_1") :> Statement<_>
            | "second" -> return FieldGet(args.[0], "_2") :> Statement<_>
            | "third" -> return FieldGet(args.[0], "_3") :> Statement<_>
            | other -> return failwithf "Unsupported call: %s" other
        }

    let private itemHelper exprs hostVar =
        translation {
            let! idx = translation {
                match exprs with
                | hd :: _ -> return! translateAsExpr hd
                | [] -> return failwith "Array index missed!"
            }

            return idx, hostVar
        }

    let private translateSpecificPropGet expr propName exprs =
        translation {
            // TODO: Refactoring: Safe pattern matching by expr type.
            let! hostVar = translateAsExpr expr
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
            // TODO rewrite to active pattern
            | "value" when
                match expr with
                | Patterns.Var v -> Some v
                | _ -> None
                |> Option.exists (fun v -> v.Type.Name.ToLower().StartsWith ClCell_) ->

                let! (idx, hVar) = itemHelper [Expr.Value 0] hostVar
                return Item(hVar, idx) :> Expression<_>
            | _ -> return failwithf "Unsupported property in kernel: %A" propName
        }

    let private translatePropGet (exprOpt: Expr Option) (propInfo: PropertyInfo) exprs =
        translation {
            let propName = propInfo.Name.ToLowerInvariant()

            match exprOpt with
            | Some expr ->
                match! State.gets (fun context -> context.UserDefinedTypes.Contains expr.Type) with
                | true ->
                    let exprTypeName = expr.Type.Name.ToLowerInvariant()
                    match! State.gets (fun context ->  context.UserDefinedStructsDecls.ContainsKey exprTypeName) with
                    | true -> return! translateStructFieldGet expr propInfo.Name
                    | false -> return! translateUnionFieldGet expr propInfo
                | false -> return! translateSpecificPropGet expr propName exprs
            | None ->
                match propName with
                | "_localid0" ->
                    return FunCall("get_local_id", [Const(PrimitiveType Int, "0")]) :> Expression<_>
                | _ -> return failwithf "Unsupported static property get in kernel: %A" propName
        }

    let private translatePropSet exprOpt (propInfo: System.Reflection.PropertyInfo) exprs newVal =
        translation {
            // Todo: Safe pattern matching (item) by expr type
            let propName = propInfo.Name.ToLowerInvariant()

            match exprOpt with
            | Some expr ->
                let! hostVar = translateAsExpr expr
                let! newVal = translateAsExpr newVal

                return! translation {
                    match propInfo.Name.ToLowerInvariant() with
                    | "item" ->
                        let! (idx, hVar) = itemHelper exprs hostVar
                        let item = Item(hVar, idx)
                        return Assignment(Property(PropertyType.Item item), newVal) :> Statement<_>
                    // TODO rewrite to active pattern
                    | "value" when
                        match expr with
                        | Patterns.Var v -> Some v
                        | _ -> None
                        |> Option.exists (fun v -> v.Type.Name.ToLower().StartsWith ClCell_) ->

                        let! (idx, hVar) = itemHelper [Expr.Value 0] hostVar
                        let item = Item(hVar, idx)
                        return Assignment(Property(PropertyType.Item item), newVal) :> Statement<_>
                    | _ ->
                        let! translated = translateFieldSet expr propInfo.Name exprs.[0]
                        return translated :> Statement<_>
                }
            | None -> return failwithf "Unsupported static property set in kernel: %A" propName
        }

    let translateAsExpr expr =
        translation {
            let! (translated: Node<_>) = translate expr
            return (translated :?> Expression<_>)
        }

    let getVar (clVarName: string) =
        translation {
            return Variable clVarName
        }

    let translateVar (var: Var) =
        translation {
            //getVar var.Name targetContext
            match! State.gets (fun context -> context.Namer.GetCLVarName var.Name) with
            | Some varName -> return! getVar varName
            | None ->
                return failwithf
                    "Seems, that you try to use variable with name %A, that declared out of quotation. \
                    Please, pass it as quoted function's parametaer." var.Name
        }

    let translateValue (value: obj) (sType: System.Type) =
        translation {
            match sType.Name.ToLowerInvariant() with
            | "boolean" ->
                let! translatedType = Type.translate sType
                let stringValue = if value.ToString().ToLowerInvariant() = "false" then "0" else "1"
                return translatedType, stringValue

            | typeName when typeName.EndsWith "[]" ->
                let array =
                    match typeName with
                    | "int32[]" -> value :?> array<int> |> Array.map string
                    | "byte[]" -> value :?> array<byte> |> Array.map string
                    | "single[]" -> value :?> array<float32> |> Array.map string
                    | _ -> failwith "Unsupported array type."

                let! translatedType = Type.translate sType |> State.using (fun ctx -> { ctx with AKind = ArrayArray array.Length })
                let stringValue =
                    array
                    |> String.concat ", "
                    |> fun s -> "{ " + s + "}"

                return translatedType, stringValue

            | _ ->
                let! translatedType = Type.translate sType
                // string null = ""
                let stringValue = string value
                return translatedType, stringValue
        }
        |> State.map (fun (type', value) -> Const(type', value))

    let translateVarSet (var: Var) (expr: Expr) =
        translation {
            let! var = translateVar var
            let! expr = translateCond (*TranslateAsExpr*) expr
            return Assignment(Property(PropertyType.Var var), expr)
        }

    let translateCond (cond: Expr) =
        translation {
            match cond with
            | Patterns.IfThenElse (if', then', else') ->
                let! l = translateCond if'
                let! r = translateCond then'
                let! e = translateCond else'
                let! isBoolAsBit = State.gets (fun context -> context.TranslatorOptions |> List.contains BoolAsBit)
                let o1 =
                    match r with
                    | :? Const<Lang> as c when c.Val = "1" -> l
                    | _ -> Binop((if isBoolAsBit then BitAnd else And), l, r) :> Expression<_>

                match e with
                | :? Const<Lang> as c when c.Val = "0" ->
                    return o1
                | _ -> return Binop((if isBoolAsBit then BitOr else Or), o1, e) :> Expression<_>

            | _ -> return! translateAsExpr cond
        }

    let toStb (s: Node<_>) =
        translation {
            match s with
            | :? StatementBlock<_> as s ->
                return s
            | x -> return StatementBlock <| ResizeArray [x :?> Statement<_>]
        }

    let translateIf (cond: Expr) (thenBranch: Expr) (elseBranch: Expr) =
        translation {
            let! if' = translateCond cond
            let! then' = translate thenBranch >>= toStb |> State.using clearContext
            let! else' = translation {
                match elseBranch with
                | Patterns.Value (null, sType) -> return None
                | _ ->
                    return!
                        translate elseBranch >>= toStb
                        |> State.using clearContext
                        |> State.map Some
            }

            return IfThenElse(if', then', else')
        }

    // TODO refac
    let translateForIntegerRangeLoop (i: Var) (from': Expr) (to': Expr) (loopBody: Expr) =
        translation {
            let! iName = State.gets (fun context -> context.Namer.LetStart i.Name)
            let! v = getVar iName
            let! var = translateBinding i iName from'
            let! condExpr = translateAsExpr to'
            do! State.modify (fun context -> context.Namer.LetIn i.Name; context)
            let! body = translate loopBody >>= toStb |> State.using clearContext
            let cond = Binop(LessEQ, v, condExpr)
            let condModifier = Unop(UOp.Incr, v)
            do! State.modify (fun context -> context.Namer.LetOut(); context)
            return ForIntegerLoop(var, cond, condModifier, body)
        }

    let translateWhileLoop condExpr bodyExpr =
        translation {
            let! nCond = translateCond condExpr
            let! nBody = translate bodyExpr >>= toStb
            return WhileLoop(nCond, nBody)
        }

    let translateSeq expr1 expr2 =
        translation {
            let linearized = ResizeArray()
            let rec go expr =
                match expr with
                | Patterns.Sequential (e1, e2) ->
                    go e1
                    go e2
                | _ -> linearized.Add expr

            go expr1
            go expr2

            let! decls = State.gets (fun context -> ResizeArray(context.VarDecls))
            do! State.modify (fun context -> context.VarDecls.Clear(); context)

            for expr in linearized do
                do! State.modify (fun context -> context.VarDecls.Clear(); context)
                match! translate expr with
                | :? StatementBlock<Lang> as s1 ->
                    decls.AddRange(s1.Statements)
                | s1 -> decls.Add(s1 :?> Statement<_>)

            return StatementBlock decls
        }

    // TODO change to lambdas and applications without rec
    let translateApplication expr1 expr2 =
        translation {
            let rec go expr vals args =
                match expr with
                | Patterns.Lambda (v, e) -> go e vals (v :: args)
                | Patterns.Application (e1, e2) -> go e1 (e2 :: vals) args
                | _ ->
                    if vals.Length = args.Length then
                        let argsDict =
                            vals
                            |> List.zip (List.rev args)
                            |> dict

                        //failwith "Partial evaluation is not supported in kernel function."
                        expr.Substitute(fun v -> if argsDict.ContainsKey v then Some argsDict.[v] else None), true
                    else
                        expr, false


            let (body, doing) = go expr1 [expr2] []
            return body, doing
        }

    // TODO change to applications without rec
    let translateApplicationFun expr1 expr2 =
        translation {
            let rec go expr vals = translation {
                match expr with
                | Patterns.Application (e1, e2) ->
                    let! exp = translateAsExpr e2
                    return! go e1 (exp :: vals)
                | _ ->
                    // TODO fix it: return exception rather than expr.ToString()
                    // NOTE не поддерживается частичное применение
                    // NOTE не поддерживается композиция функций (или функции высшего порядка)
                    let funName =
                        match expr with
                        | Patterns.ValueWithName (_, _, name) -> name
                        | _ -> expr.ToString()

                    return FunCall(funName, vals) :> Statement<_>
            }

            let! exp = translateAsExpr expr2
            return! go expr1 [exp]
        }

    let translateFieldSet host name value =
        translation {
            let! hostE = translateAsExpr host
            let! valE = translateAsExpr value
            return FieldSet(hostE, name, valE)
        }

    let translateStructFieldGet host name =
        translation {
            let! hostE = translateAsExpr host
            return FieldGet(hostE, name) :> Expression<_>
        }

    let translateUnionFieldGet expr (propInfo: PropertyInfo) =
        translation {
            let exprTypeName = expr.Type.Name.ToLowerInvariant()
            let! unionType = State.gets (fun context -> context.UserDefinedUnionsDecls.[exprTypeName])

            let! unionValueExpr = translateAsExpr expr

            let caseName = propInfo.DeclaringType.Name
            let unionCaseField = unionType.GetCaseByName caseName

            match unionCaseField with
            | Some unionCaseField ->
                return
                    FieldGet(
                        FieldGet(
                            FieldGet(unionValueExpr, unionType.Data.Name),
                            unionCaseField.Name
                        ),
                        propInfo.Name
                    )
                    :> Expression<_>
            | None ->
                return failwithf
                    "Union field get translation error: union %A doesn't have case %A" unionType.Name caseName
        }

    let translate expr =
        translation {
            match expr with
            | Patterns.AddressOf expr -> return failwithf "AdressOf is not suported: %O" expr
            | Patterns.AddressSet expr -> return failwithf "AdressSet is not suported: %O" expr

            | Patterns.Application (expr1, expr2) ->
                let! (e, appling) = translateApplication expr1 expr2
                if appling then
                    return! translate e
                else
                    let! r = translateApplicationFun expr1 expr2
                    return r :> Node<_>

            | DerivedPatterns.SpecificCall <@@ PrintfReplacer.print @@> (_, _, args) ->
                match args with
                | [ Patterns.ValueWithName (argTypes, _, _);
                    Patterns.ValueWithName (formatStr, _, _);
                    Patterns.ValueWithName (argValues, _, _) ] ->

                    let formatStrArg = Const(PrimitiveType ConstStringLiteral, formatStr :?> string) :> Expression<_>
                    let! args' = translateListOfArgs (argValues :?> list<Expr>)
                    return FunCall("printf", formatStrArg :: args') :> Node<_>
                | _ -> return failwith "printf: something going wrong."

            | DerivedPatterns.SpecificCall <@ (|>) @>
                (
                    _,
                    _,
                    [expr; Patterns.Lambda(_, DerivedPatterns.SpecificCall <@ ignore @> (_, _, _))]
                ) ->
                return! translate expr

            | Patterns.Call (exprOpt, mInfo, args) ->
                let! r = translateCall exprOpt mInfo args
                return r :> Node<_>
            | Patterns.Coerce (expr, sType) -> return failwithf "Coerce is not suported: %O" expr
            | Patterns.DefaultValue sType -> return failwithf "DefaulValue is not suported: %O" expr
            | Patterns.FieldGet (exprOpt, fldInfo) ->
                match exprOpt with
                | Some expr ->
                    let! r = translateStructFieldGet expr fldInfo.Name
                    return r :> Node<_>
                | None -> return failwithf "FieldGet for empty host is not suported. Field: %A" fldInfo.Name
            | Patterns.FieldSet (exprOpt, fldInfo, expr) ->
                match exprOpt with
                | Some e ->
                    let! r = translateFieldSet e fldInfo.Name expr
                    return r :> Node<_>
                | None -> return failwithf "Fileld set with empty host is not supported. Field: %A" fldInfo
            | Patterns.ForIntegerRangeLoop (i, from, _to, _do) ->
                let! r = translateForIntegerRangeLoop i from _to _do
                return r :> Node<_>
            | Patterns.IfThenElse (cond, thenExpr, elseExpr) ->
                let! r = translateIf cond thenExpr elseExpr
                return r :> Node<_>
            | Patterns.Lambda (var, _expr) ->
                // translateLambda var expr targetContext
                return failwithf "Lambda is not suported: %A" expr
            | Patterns.Let (var, expr, inExpr) ->
                match var.Name with
                | "___providedCallInfo" -> return! translateProvidedCall expr
                | _ -> return! translateLet var expr inExpr

            | Patterns.LetRecursive (bindings, expr) -> return failwithf "LetRecursive is not suported: %O" expr
            | Patterns.NewArray (sType, exprs) -> return failwithf "NewArray is not suported: %O" expr
            | Patterns.NewDelegate (sType, vars, expr) -> return failwithf "NewDelegate is not suported: %O" expr
            | Patterns.NewObject (constrInfo, exprs) ->
                let! context = State.get
                let p = constrInfo.GetParameters()
                let p2 = constrInfo.GetMethodBody()
                let! flag = State.gets (fun context -> context.UserDefinedTypes.Contains(constrInfo.DeclaringType))
                if flag then
                    let! structInfo = State.gets (fun context -> context.UserDefinedStructsDecls.[constrInfo.DeclaringType.Name.ToLowerInvariant()])
                    let cArgs = exprs |> List.map (fun x -> translation { return! translateAsExpr x })
                    let res = NewStruct<_>(structInfo, cArgs |> List.map (State.eval context))
                    return res :> Node<_>
                else
                    return failwithf "NewObject is not suported: %O" expr
            | Patterns.NewRecord (sType, exprs) -> return failwithf "NewRecord is not suported: %O" expr
            | Patterns.NewTuple (exprs) ->
                let! context = State.get
                let mutable n = 0
                let baseTypes = [| for i in 0 .. exprs.Length - 1 -> exprs.[i].Type |]
                let elements =
                    [
                        for i in 0 .. exprs.Length - 1 ->
                            {
                                Name = "_" + (i + 1).ToString()
                                Type = Type.translate baseTypes.[i] |> State.eval context
                            }
                    ]
                let mutable s = ""
                for i in 0 .. baseTypes.Length - 1 do
                    s <- s + baseTypes.[i].Name
                if not (context.TupleDecls.ContainsKey(s)) then
                    let! index = State.gets (fun ctx -> ctx.TupleDecls.Count)
                    let tupleDecl = StructType(sprintf "tuple%i" index, elements)
                    do! State.modify (fun ctx -> ctx.TupleDecls.Add(s, tupleDecl); ctx)
                    let cArgs = exprs |> List.map (fun x -> translateAsExpr x)

                    let! context = State.get
                    return NewStruct<_>(tupleDecl, cArgs |> List.map (State.eval context)) :> Node<_>
                else
                    let! tupleDecl = State.gets (fun ctx -> ctx.TupleDecls.[s])
                    let cArgs = exprs |> List.map (fun x -> translateAsExpr x)

                    let! context = State.get
                    return NewStruct<_>(tupleDecl, cArgs |> List.map (State.eval context)) :> Node<_>
            | Patterns.NewUnionCase (unionCaseInfo, exprs) ->
                let! context = State.get
                let unionType = unionCaseInfo.DeclaringType
                if not <| context.UserDefinedTypes.Contains(unionType) then
                    failwithf "Union type %s is not registered" unionType.Name

                let typeName = unionType.Name.ToLowerInvariant()
                let unionInfo = context.UserDefinedUnionsDecls.[typeName]

                let tag = Const(unionInfo.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>
                let args =
                    match unionInfo.GetCaseByTag unionCaseInfo.Tag with
                    | None -> []
                    | Some field ->
                        let structArgs = exprs |> List.map (fun x -> translateAsExpr x) |> List.map (State.eval context)
                        let data =
                            NewUnion(
                                unionInfo.Data.Type :?> UnionClInplaceType<_>,
                                field.Name,
                                NewStruct(field.Type :?> StructType<_>, structArgs)
                            )
                        [ data :> Expression<_> ]

                return NewStruct(unionInfo, tag :: args) :> Node<_>
            | Patterns.PropertyGet (exprOpt, propInfo, exprs) ->
                let! res = translatePropGet exprOpt propInfo exprs
                return res :> Node<_>
            | Patterns.PropertySet (exprOpt, propInfo, exprs, expr) ->
                let! res = translatePropSet exprOpt propInfo exprs expr
                return res :> Node<_>
            | Patterns.Sequential (expr1, expr2) ->
                let! res = translateSeq expr1 expr2
                return res :> Node<_>
            | Patterns.TryFinally (tryExpr, finallyExpr) -> return failwithf "TryFinally is not suported: %O" expr
            | Patterns.TryWith (expr1, var1, expr2, var2, expr3) -> return failwithf "TryWith is not suported: %O" expr
            | Patterns.TupleGet (expr, i) ->
                let! r = translateStructFieldGet expr ("_" + (string (i + 1)))
                return r :> Node<_>
            | Patterns.TypeTest (expr, sType) -> return failwithf "TypeTest is not suported: %O" expr
            | Patterns.UnionCaseTest (expr, unionCaseInfo) ->
                let! context = State.get
                let unionTypeName = expr.Type.Name.ToLowerInvariant()
                let unionDecl = context.UserDefinedUnionsDecls.[unionTypeName]

                let! unionVarExpr = translateAsExpr expr
                let unionGetTagExpr = FieldGet(unionVarExpr, unionDecl.Tag.Name) :> Expression<_>
                let tagExpr = Const(unionDecl.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>

                return Binop(BOp.EQ, unionGetTagExpr, tagExpr) :> Node<_>
            | Patterns.ValueWithName (_obj, sType, name) ->
                let! context = State.get
                // Here is the only use of TranslationContext.InLocal
                if sType.ToString().EndsWith "[]" (*&& not context.InLocal*) then
                    context.Namer.AddVar name
                    let! res = translateValue _obj sType
                    context.TopLevelVarsDecls.Add(
                        VarDecl(res.Type, name, Some(res :> Expression<_>), AddressSpaceQualifier.Constant)
                    )
                    let var = Var(name, sType)
                    let! res = translateVar var
                    return res :> Node<_>
                else
                    let! res = translateValue _obj sType
                    return res :> Node<_>
            | Patterns.Value (_obj, sType) ->
                let! res = translateValue _obj sType
                return res :> Node<_>
            | Patterns.Var var ->
                let! res = translateVar var
                return res :> Node<_>
            | Patterns.VarSet (var, expr) ->
                let! res = translateVarSet var expr
                return res :> Node<_>
            | Patterns.WhileLoop (condExpr, bodyExpr) ->
                let! r = translateWhileLoop condExpr bodyExpr
                return r :> Node<_>
            | _ -> return failwithf "OTHER!!! : %O" expr
        }

    let private translateLet var expr inExpr = translation {
        let! bName = State.gets (fun context -> context.Namer.LetStart var.Name)

        let! vDecl = translation {
            match expr with
            | DerivedPatterns.SpecificCall <@@ local @@> (_, _, _) ->
                let! vType = Type.translate var.Type
                return VarDecl(vType, bName, None, spaceModifier = Local)
            | DerivedPatterns.SpecificCall <@@ localArray @@> (_, _, [arg]) ->
                let! expr = translateCond arg
                let arrayLength =
                    match expr with
                    | :? Const<Lang> as c -> int c.Val
                    | other -> failwithf "Calling localArray with a non-const argument %A" other
                let! arrayType = Type.translate var.Type |> State.using (fun ctx -> { ctx with AKind = ArrayArray arrayLength })
                return VarDecl(arrayType, bName, None, spaceModifier = Local)
            | Patterns.DefaultValue _ ->
                let! vType = Type.translate var.Type
                return VarDecl(vType, bName, None)
            | _ -> return! translateBinding var bName expr
        }

        do! State.modify (fun context -> context.VarDecls.Add vDecl; context)
        do! State.modify (fun context -> context.Namer.LetIn var.Name; context)

        let! sb = State.gets (fun context -> context.VarDecls)
        let! res = translate inExpr |> State.using clearContext

        match res with
        | :? StatementBlock<Lang> as s -> sb.AddRange s.Statements
        | _ -> sb.Add(res :?> Statement<_>)

        do! State.modify (fun context -> context.Namer.LetOut(); context)

        return StatementBlock sb :> Node<_>
    }

    let private translateProvidedCall expr =
        translation {
            let rec traverse expr args = translation {
                match expr with
                | Patterns.Value (calledName, sType) ->
                    match sType.Name.ToLowerInvariant() with
                    | "string" -> return (calledName :?> string), args
                    | _ -> return failwithf "Failed to parse provided call, expected string call name: %O" expr
                | Patterns.Sequential (expr1, expr2) ->
                    let! updatedArgs = translation {
                        match expr2 with
                        | Patterns.Value (null, _) -> return args // the last item in the sequence is null
                        | _ ->
                            let! a = translateAsExpr expr2
                            return a :: args
                    }
                    return! traverse expr1 updatedArgs
                | _ -> return "Failed to parse provided call: " + string expr |> failwith
            }

            let! m = traverse expr []
            return FunCall m :> Node<_>
        }
