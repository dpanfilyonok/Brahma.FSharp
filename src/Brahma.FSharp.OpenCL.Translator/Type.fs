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

open Brahma.FSharp.OpenCL.AST
open System.Reflection
open FSharp.Reflection
open Microsoft.FSharp.Collections

module Type =
    let printElementType (_type: string) (context: TargetContext<_, _>) =
        let pType =
            match _type.ToLowerInvariant() with
            | "int"
            | "int32" -> PrimitiveType<Lang>(Int)
            | "int16" -> PrimitiveType<Lang>(Short)
            | "uint16" -> PrimitiveType<Lang>(UShort)
            | "uint32" -> PrimitiveType<Lang>(UInt)
            | "float32"
            | "single" -> PrimitiveType<Lang>(Float)
            | "byte" -> PrimitiveType<Lang>(UChar)
            | "int64" -> PrimitiveType<Lang>(Long)
            | "uint64" -> PrimitiveType<Lang>(ULong)
            | "boolean" -> PrimitiveType<Lang>(Bool)
            | "float"
            | "double" ->
                context.Flags.enableFP64 <- true
                PrimitiveType<Lang>(Double)
            | other -> failwithf "Unsuported tuple type: %s" other

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
        | other -> failwithf "Unsuported tuple type: %O" other

    let rec translate (_type: System.Type) isKernelArg size =
        translation {
            let rec go (str: string) = translation {
                match str.ToLowerInvariant() with
                | "int"
                | "int32" -> return PrimitiveType<Lang>(Int) :> Type<Lang>
                | "int16" -> return PrimitiveType<Lang>(Short) :> Type<Lang>
                | "uint16" -> return PrimitiveType<Lang>(UShort) :> Type<Lang>
                | "uint32" -> return PrimitiveType<Lang>(UInt) :> Type<Lang>
                | "float32"
                | "single" -> return PrimitiveType<Lang>(Float) :> Type<Lang>
                | "byte" -> return PrimitiveType<Lang>(UChar) :> Type<Lang>
                | "int64" -> return PrimitiveType<Lang>(Long) :> Type<Lang>
                | "uint64" -> return PrimitiveType<Lang>(ULong) :> Type<Lang>
                | "boolean" -> return PrimitiveType<Lang>(Bool) :> Type<Lang>
                | "float"
                | "double" ->
                    do! TranslationContext.modify (fun context -> context.Flags.enableFP64 <- true; context)
                    return PrimitiveType<Lang>(Double) :> Type<Lang>
                | "unit" -> return PrimitiveType<Lang>(Void) :> Type<Lang>
                | "read_only image2D" -> return Image2DType(true) :> Type<Lang>
                | "write_only image2D" -> return Image2DType(false) :> Type<Lang>
                | t when t.EndsWith "[]" ->
                    let baseT = t.Substring(0, t.Length - 2)
                    if isKernelArg then
                        let! t = go baseT
                        return RefType(t, []) :> Type<Lang>
                    else
                        let! t = go baseT
                        return ArrayType(t, size |> Option.get) :> Type<Lang>
                | s when s.StartsWith "fsharpref" ->
                    let! t = go (_type.GetGenericArguments().[0].Name)
                    return RefType(t, []) :> Type<Lang>
                | f when f.StartsWith "fsharpfunc" ->
                    //            go (_type.GetGenericArguments().[1].Name)
                    return! translate (_type.GetGenericArguments().[1]) isKernelArg size
                | tp when tp.Contains("tuple") ->
                    let types =
                        if _type.Name.EndsWith("[]") then
                            _type
                                .UnderlyingSystemType
                                .ToString()
                                .Substring(15, _type.UnderlyingSystemType.ToString().Length - 18)
                                .Split(',')
                        else
                            _type
                                .UnderlyingSystemType
                                .ToString()
                                .Substring(15, _type.UnderlyingSystemType.ToString().Length - 16)
                                .Split(',')
                    let mutable n = 0
                    let baseTypes = [| for i in 0 .. types.Length - 1 -> types.[i].Substring(7) |]
                    let elements =
                        [
                            for i in 0 .. types.Length - 1 -> { Name = "_" + (i + 1).ToString(); Type = go baseTypes.[i] }
                        ]
                    let mutable s = ""
                    for i in 0 .. baseTypes.Length - 1 do
                        s <- s + baseTypes.[i]
                    if not (context.TupleDecls.ContainsKey(s)) then
                        context.TupleNumber <- context.TupleNumber + 1
                        n <- context.TupleNumber
                        context.TupleDecls.Add(s, n)
                        let a = StructType("tuple" + n.ToString(), elements)
                        context.TupleList.Add(a)
                        TupleType(a, n) :> Type<_>
                    else
                        n <- context.TupleDecls.Item(s)
                        let a = StructType("tuple" + n.ToString(), elements)
                        TupleType(a, n) :> Type<_>
                | x when context.UserDefinedTypes.Exists(fun t -> t.Name.ToLowerInvariant() = x) ->
                    let structType =
                        if context.UserDefinedStructsOpenCLDeclaration.ContainsKey x then
                            context.UserDefinedStructsOpenCLDeclaration.[x]
                        elif context.UserDefinedUnionsOpenCLDeclaration.ContainsKey x then
                            context.UserDefinedUnionsOpenCLDeclaration.[x] :> StructType<_>
                        else
                            failwithf "Declaration of struct %s doesn't exists" x
                    structType :> Type<_>
                | other -> failwithf "Unsupported kernel type: %s" other
            }

            return! go _type.Name
        }

    let translateStructDecls structs =
        translation {
            let translateStruct (t: System.Type) = translation {
                let name = t.Name
                let! fields =
                    [
                        for f in t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) ->
                            translate f.PropertyType true None >>= fun type' ->
                            Translation.return' { Name = f.Name; Type = type' }
                    ]
                    @
                    [
                        for f in t.GetFields(BindingFlags.Public ||| BindingFlags.Instance) ->
                            translate f.FieldType true None >>= fun type' ->
                            Translation.return' { Name = f.Name; Type = type' }
                    ]
                    |> Translation.collect

                return StructType(name, fields)
            }

            do! TranslationContext.modify (fun context -> context.UserDefinedTypes.AddRange(structs); context)

            return structs
            |> List.map
                (fun t ->
                    translation {
                        let! r = translateStruct t
                        do! TranslationContext.modify (fun context -> context.UserDefinedStructsOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), r); context)
                        return StructDecl r
                    }
                )
           |> Translation.collect
        }

    let translateDiscriminatedUnionDecls (unions: List<System.Type>) =
        translation {
            let translateUnion (t: System.Type) = translation {
                let name = t.Name

                let notEmptyCases =
                    FSharpType.GetUnionCases t
                    |> Array.filter (fun case -> case.GetFields().Length <> 0)

                let! fields =
                    [
                        for case in notEmptyCases ->
                            translation {
                                let structName = case.Name
                                let tag = case.Tag
                                let! fields =
                                    [
                                        for field in case.GetFields() ->
                                            translate field.PropertyType false None >>= fun type' ->
                                            Translation.return' { Name = field.Name; Type = type' }
                                    ]
                                    |> Translation.collect

                                return tag, { Name = structName; Type = StructInplaceType(structName + "Type", fields) }
                            }

                    ]
                    |> Translation.collect

                return DiscriminatedUnionType(name, fields)
            }

            return unions
            |> List.map
                (fun t ->
                    translation {
                        let! u = translateUnion t
                        do! TranslationContext.modify <| fun context ->
                            context.UserDefinedTypes.Add(t)
                            context.UserDefinedUnionsOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), u)
                            context
                        return StructDecl u
                    }
                )
            |> Translation.collect
        }
