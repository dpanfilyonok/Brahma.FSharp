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

module Brahma.FSharp.OpenCL.Translator.Type

open Brahma.FSharp.OpenCL.AST
open System.Reflection
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open System.Collections.Generic

let printElementType (_type: string) (context:TargetContext<_,_>) =
    let pType =
        match _type.ToLowerInvariant() with
        | "int"| "int32" -> PrimitiveType<Lang>(Int)
        | "int16" -> PrimitiveType<Lang>(Short)
        | "uint16" -> PrimitiveType<Lang>(UShort)
        | "uint32" -> PrimitiveType<Lang>(UInt)
        | "float32" | "single"-> PrimitiveType<Lang>(Float)
        | "byte" -> PrimitiveType<Lang>(UChar)
        | "int64" -> PrimitiveType<Lang>(Long)
        | "uint64" -> PrimitiveType<Lang>(ULong)
        | "boolean" -> PrimitiveType<Lang>(Bool)
        | "float" | "double" ->
            context.Flags.enableFP64 <- true
            PrimitiveType<Lang>(Double)
        | x -> "Unsuported tuple type: " + x |> failwith
    match pType.Type with
    | UChar -> "uchar"
    | Short -> "short"
    | UShort -> "ushort"
    | Int -> "int"
    | UInt -> "uint"
    | Float -> "float"
    | Long -> "long"
    | ULong -> "ulong"
    | Double -> "double"
    | Bool -> "bool"
    | x -> "Unsuported tuple type: " + x.ToString() |> failwith


let rec Translate (_type:System.Type) isKernelArg size (context:TargetContext<_,_>) : Type<Lang> =
    let rec go (str:string)=
        let mutable low = str.ToLowerInvariant()
        match low with
        | "int"| "int32" -> PrimitiveType<Lang>(Int) :> Type<Lang>
        | "int16" -> PrimitiveType<Lang>(Short) :> Type<Lang>
        | "uint16" -> PrimitiveType<Lang>(UShort) :> Type<Lang>
        | "uint32" -> PrimitiveType<Lang>(UInt) :> Type<Lang>
        | "float32" | "single"-> PrimitiveType<Lang>(Float) :> Type<Lang>
        | "byte" -> PrimitiveType<Lang>(UChar) :> Type<Lang>
        | "int64" -> PrimitiveType<Lang>(Long) :> Type<Lang>
        | "uint64" -> PrimitiveType<Lang>(ULong) :> Type<Lang>
        | "boolean" -> PrimitiveType<Lang>(Bool) :> Type<Lang>
        | "float" | "double" ->
            context.Flags.enableFP64 <- true
            PrimitiveType<Lang>(Double) :> Type<Lang>
        | "unit" -> PrimitiveType<Lang>(Void) :> Type<Lang>
        | "read_only image2D" -> Image2DType(true) :> Type<Lang>
        | "write_only image2D" -> Image2DType(false) :> Type<Lang>
        | t when t.EndsWith "[]" ->
            let baseT = t.Substring(0,t.Length-2)
            if isKernelArg
            then RefType<_>(go baseT, []) :> Type<Lang>
            else ArrayType<_>(go baseT, size |> Option.get) :> Type<Lang>
        | s when s.StartsWith "fsharpref" ->
            go (_type.GetGenericArguments().[0].Name)
        | f when f.StartsWith "fsharpfunc" ->
//            go (_type.GetGenericArguments().[1].Name)
            Translate (_type.GetGenericArguments().[1]) isKernelArg size context
        | tp when tp.Contains ("tuple") ->
             let types =
                if _type.Name.EndsWith("[]") then  _type.UnderlyingSystemType.ToString().Substring(15, _type.UnderlyingSystemType.ToString().Length - 18).Split(',')
                else _type.UnderlyingSystemType.ToString().Substring(15, _type.UnderlyingSystemType.ToString().Length - 16).Split(',')
             let mutable n = 0
             let baseTypes = [|for i in 0..types.Length - 1 -> types.[i].Substring(7)|]
             let elements = [for i in 0..types.Length - 1 -> { Name = "_" + (i + 1).ToString(); Type = go baseTypes.[i] }]
             let mutable s = ""
             for i in 0..baseTypes.Length - 1 do s <- s + baseTypes.[i]
             if not (context.tupleDecls.ContainsKey(s))
             then
                 context.tupleNumber <- context.tupleNumber + 1
                 n <- context.tupleNumber
                 context.tupleDecls.Add(s, n)
                 let a = StructType<_>("tuple" + n.ToString(), elements)
                 context.tupleList.Add(a)
                 TupleType<_>(a, n) :> Type<_>
             else
                 n <- context.tupleDecls.Item(s)
                 let a = StructType<_>("tuple" + n.ToString(), elements)
                 TupleType<_>(a, n) :> Type<_>
        | x when context.UserDefinedTypes.Exists(fun t -> t.Name.ToLowerInvariant() = x)
            ->
                if not <| context.UserDefinedTypesOpenCLDeclaration.ContainsKey x
                    then failwithf "Declaration of struct %s doesn't exists" x
                let structType =  context.UserDefinedTypesOpenCLDeclaration.[x]
                structType :> Type<_>
        | x -> "Unsupported kernel type: " + x |> failwith
    _type.Name
    |> go


let TranslateStructDecls structs (targetContext:TargetContext<_,_>) =
    let translateStruct (t:System.Type) =
        let name = t.Name
        let fields = [ for f in
                            t.GetProperties (BindingFlags.Public ||| BindingFlags.Instance) ->
                            { Name = f.Name; Type = Translate f.PropertyType true None targetContext }]
                     @
                     [ for f in
                            t.GetFields(BindingFlags.Public ||| BindingFlags.Instance) ->
                            { Name = f.Name; Type = Translate f.FieldType true None targetContext }]

        StructType<_>(name, fields)

    let translated =
        do targetContext.UserDefinedTypes.AddRange(structs)
        structs
        |> List.map
            (fun t ->
                let r = translateStruct t
                targetContext.UserDefinedTypesOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), r)
                StructDecl r)
    translated
