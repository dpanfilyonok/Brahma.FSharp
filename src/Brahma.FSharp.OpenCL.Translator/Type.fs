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
    let rec translate (type': System.Type) isKernelArg size (context: TargetContext<_, _>) : Type<Lang> =
        let rec go (str: string) =
            let mutable low = str.ToLowerInvariant()
            match low with
            | "int"
            | "int32" -> PrimitiveType<Lang>(Int) :> Type<Lang>
            | "int16" -> PrimitiveType<Lang>(Short) :> Type<Lang>
            | "uint16" -> PrimitiveType<Lang>(UShort) :> Type<Lang>
            | "uint32" -> PrimitiveType<Lang>(UInt) :> Type<Lang>
            | "float32"
            | "single" -> PrimitiveType<Lang>(Float) :> Type<Lang>
            | "byte" -> PrimitiveType<Lang>(UChar) :> Type<Lang>
            | "int64" -> PrimitiveType<Lang>(Long) :> Type<Lang>
            | "uint64" -> PrimitiveType<Lang>(ULong) :> Type<Lang>
            | "boolean" ->
                if context.TranslatorOptions |> Array.contains UseNativeBooleanType then
                    PrimitiveType<Lang>(Bool) :> Type<Lang>
                else
                    PrimitiveType<Lang>(BoolClAlias) :> Type<Lang>
            | "float"
            | "double" ->
                context.Flags.enableFP64 <- true
                PrimitiveType<Lang>(Double) :> Type<Lang>
            | "unit" -> PrimitiveType<Lang>(Void) :> Type<Lang>
            | "read_only image2D" -> Image2DType(true) :> Type<Lang>
            | "write_only image2D" -> Image2DType(false) :> Type<Lang>
            | t when t.EndsWith "[]" ->
                let baseT = t.Substring(0, t.Length - 2)
                if isKernelArg then
                    RefType(go baseT, []) :> Type<Lang>
                else
                    // NOTE why ArrayType is different from RefType from C lang perspective
                    ArrayType(go baseT, size |> Option.get) :> Type<Lang>
            | s when s.StartsWith Buffer ->
                let baseT = type'.GetGenericArguments().[0].Name
                if isKernelArg then
                    RefType(go baseT, []) :> Type<Lang>
                else
                    // NOTE why ArrayType is different from RefType from C lang perspective
                    ArrayType(go baseT, size |> Option.get) :> Type<Lang>

            | s when s.StartsWith "fsharpref" -> RefType(go (type'.GetGenericArguments().[0].Name), []) :> Type<Lang>
            | f when f.StartsWith "fsharpfunc" ->
                //            go (_type.GetGenericArguments().[1].Name)
                translate (type'.GetGenericArguments().[1]) isKernelArg size context
            | tp when tp.Contains("tuple") ->
                let types =
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

        go type'.Name

    let translateStructDecls structs (targetContext: TargetContext<_, _>) =
        let translateStruct (t: System.Type) =
            let name = t.Name
            let fields =
                [
                    for f in t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) ->
                        { Name = f.Name; Type = translate f.PropertyType true None targetContext }
                ]
                @
                [
                    for f in t.GetFields(BindingFlags.Public ||| BindingFlags.Instance) ->
                        { Name = f.Name; Type = translate f.FieldType true None targetContext }
                ]

            StructType(name, fields)

        let translated =
            targetContext.UserDefinedTypes.AddRange(structs)
            structs
            |> List.map (fun t ->
                let r = translateStruct t
                targetContext.UserDefinedStructsOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), r)
                StructDecl r
            )
        translated

    let translateDiscriminatedUnionDecls (unions: List<System.Type>) (tc: TargetContext<_, _>) =
        let translateUnion (t: System.Type) =
            let name = t.Name

            let notEmptyCases =
                FSharpType.GetUnionCases t
                |> Array.filter (fun case -> case.GetFields().Length <> 0)

            let fields =
                [
                    for case in notEmptyCases ->
                        let structName = case.Name
                        let tag = case.Tag
                        let fields : List<Field<_>> =
                            [
                                for field in case.GetFields() ->
                                    { Name = field.Name; Type = translate field.PropertyType false None tc }
                            ]

                        tag, { Name = structName; Type = StructInplaceType(structName + "Type", fields) }
                ]
            DiscriminatedUnionType(name, fields)

        unions
        |> List.map (fun t ->
            let u = translateUnion t
            tc.UserDefinedTypes.Add(t)
            tc.UserDefinedUnionsOpenCLDeclaration.Add(t.Name.ToLowerInvariant(), u)
            StructDecl u
        )
