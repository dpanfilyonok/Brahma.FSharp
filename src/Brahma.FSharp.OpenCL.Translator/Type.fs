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
    // как указатель транслируем только массивы и refType
    let rec translate (type': System.Type) isKernelArg size =
        translation {
            let rec go (str: string) = translation {
                let mutable low = str.ToLowerInvariant()
                match low with
                | "int"
                | "int32" -> return PrimitiveType<Lang>(Int) :> Type<Lang>
                | "int16" -> return PrimitiveType<Lang>(Short) :> Type<Lang>
                | "uint16" -> return PrimitiveType<Lang>(UShort) :> Type<Lang>
                | "uint32" -> return PrimitiveType<Lang>(UInt) :> Type<Lang>
                | "float32"
                | "single" -> return PrimitiveType<Lang>(Float) :> Type<Lang>
                | "byte" -> return PrimitiveType<Lang>(UChar) :> Type<Lang>
                | "sbyte" -> return PrimitiveType<Lang>(Char) :> Type<Lang>
                | "int64" -> return PrimitiveType<Lang>(Long) :> Type<Lang>
                | "uint64" -> return PrimitiveType<Lang>(ULong) :> Type<Lang>
                | "boolean" ->
                    let! context = State.get
                    if context.TranslatorOptions |> Array.contains UseNativeBooleanType then
                        return PrimitiveType<Lang>(Bool) :> Type<Lang>
                    else
                        return PrimitiveType<Lang>(BoolClAlias) :> Type<Lang>
                | "float"
                | "double" ->
                    let! context = State.get
                    context.Flags.enableFP64 <- true
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

                | s when s.StartsWith ClArray_ || s.StartsWith ClCell_ || s.StartsWith IBuffer_ ->
                    let baseT = type'.GetGenericArguments().[0].Name
                    if isKernelArg then
                        let! t = go baseT
                        return RefType(t, []) :> Type<Lang>
                    else
                        let! t = go baseT
                        return ArrayType(t, size |> Option.get) :> Type<Lang>

                | s when s.StartsWith "fsharpref" ->
                    let! t = go (type'.GetGenericArguments().[0].Name)
                    return RefType(t, []) :> Type<Lang>
                | f when f.StartsWith "fsharpfunc" ->
                    return! translate (type'.GetGenericArguments().[1]) isKernelArg size

                // TODO переделать, тк происходит не пойми что
                | tp when tp.Contains("tuple") ->
                    let! context = State.get
                    // определяем значения типовых аргументов
                    let types =
                        // эта ветвь не нужна скорее всего
                        if type'.Name.EndsWith("[]") then
                            type'
                                .UnderlyingSystemType
                                .ToString()
                                .Substring(15, type'.UnderlyingSystemType.ToString().Length - 18)
                                .Split(',')
                        else
                            type'
                                .UnderlyingSystemType
                                .ToString()
                                .Substring(15, type'.UnderlyingSystemType.ToString().Length - 16)
                                .Split(',')
                    let mutable n = 0
                    // убираем System
                    let baseTypes = [| for i in 0 .. types.Length - 1 -> types.[i].Substring(7) |]
                    // список полей генерирумеой структуры
                    let elements =
                        [
                            for i in 0 .. types.Length - 1 -> { Name = "_" + (i + 1).ToString(); Type = go baseTypes.[i] |> State.eval context}
                        ]
                    // идентификатор для пределения типа кортежа
                    let mutable s = ""
                    for i in 0 .. baseTypes.Length - 1 do
                        s <- s + baseTypes.[i]

                    if not (context.TupleDecls.ContainsKey(s)) then
                        // храним число кортежеей
                        context.TupleNumber <- context.TupleNumber + 1
                        n <- context.TupleNumber

                        // храним мапу из зтипа кортежа в номер
                        context.TupleDecls.Add(s, n)
                        let a = StructType("tuple" + n.ToString(), elements)

                        // храним список туплов
                        context.TupleList.Add(a)
                        return TupleType(a, n) :> Type<_>
                    else
                        n <- context.TupleDecls.Item(s)
                        let a = StructType("tuple" + n.ToString(), elements)
                        return TupleType(a, n) :> Type<_>

                // | x when context.UserDefinedTypes.Exists(fun t -> t.Name.ToLowerInvariant() = x) ->
                //     let structType =
                //         if context.UserDefinedStructsOpenCLDeclaration.ContainsKey x then
                //             context.UserDefinedStructsOpenCLDeclaration.[x]
                //         elif context.UserDefinedUnionsOpenCLDeclaration.ContainsKey x then
                //             context.UserDefinedUnionsOpenCLDeclaration.[x] :> StructType<_>
                //         else
                //             failwithf "Declaration of struct %s doesn't exists" x
                //     structType :> Type<_>
                | other -> return failwithf "Unsupported kernel type: %s" other
            }

            return! go type'.Name
        }

    let translateStructDecls structs =
        translation {
            let translateStruct (t: System.Type) = translation {
                let name = t.Name
                let! fields =
                    [
                        for f in t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) ->
                            translate f.PropertyType true None >>= fun type' ->
                            State.return' { Name = f.Name; Type = type' }
                    ]
                    @
                    [
                        for f in t.GetFields(BindingFlags.Public ||| BindingFlags.Instance) ->
                            translate f.FieldType true None >>= fun type' ->
                            State.return' { Name = f.Name; Type = type' }
                    ]
                    |> State.collect

                return StructType(name, fields)
            }

            do! State.modify (fun context -> context.UserDefinedTypes.AddRange(structs); context)

            return!
                structs
                |> List.map
                    (fun t ->
                        translation {
                            let! r = translateStruct t
                            do! State.modify (fun context -> context.UserDefinedStructsOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), r); context)
                            return StructDecl r
                        }
                    )
                |> State.collect
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
                                            State.return' { Name = field.Name; Type = type' }
                                    ]
                                    |> State.collect

                                return tag, { Name = structName; Type = StructInplaceType(structName + "Type", fields) }
                            }

                    ]
                    |> State.collect

                return DiscriminatedUnionType(name, fields)
            }

            return!
                unions
                |> List.map
                    (fun t ->
                        translation {
                            let! u = translateUnion t
                            do! State.modify <| fun context ->
                                context.UserDefinedTypes.Add(t)
                                context.UserDefinedUnionsOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), u)
                                context
                            return StructDecl u
                        }
                    )
                |> State.collect
        }
