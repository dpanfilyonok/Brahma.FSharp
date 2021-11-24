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
open System.Collections.Generic

module Type =
    let (|Name|_|) (str: string) (type': System.Type) =
        match type'.Name.ToLowerInvariant() with
        | tName when tName = str -> Some Name
        | _ -> None

    let (|EndsWith|_|) (str: string) (type': System.Type) =
        match type'.Name.ToLowerInvariant() with
        | tName when tName.EndsWith str -> Some tName
        | _ -> None

    let (|StartsWith|_|) (str: string) (type': System.Type) =
        match type'.Name.ToLowerInvariant() with
        | tName when tName.StartsWith str -> Some tName
        | _ -> None

    // как указатель транслируем только массивы и refType
    let rec translate (type': System.Type) = translation {
        match type' with
        | Name "int"
        | Name "int32" -> return PrimitiveType<Lang>(Int) :> Type<Lang>
        | Name "int16" -> return PrimitiveType<Lang>(Short) :> Type<Lang>
        | Name "uint16" -> return PrimitiveType<Lang>(UShort) :> Type<Lang>
        | Name "uint32" -> return PrimitiveType<Lang>(UInt) :> Type<Lang>
        | Name "float32"
        | Name "single" -> return PrimitiveType<Lang>(Float) :> Type<Lang>
        | Name "byte" -> return PrimitiveType<Lang>(UChar) :> Type<Lang>
        | Name "sbyte" -> return PrimitiveType<Lang>(Char) :> Type<Lang>
        | Name "int64" -> return PrimitiveType<Lang>(Long) :> Type<Lang>
        | Name "uint64" -> return PrimitiveType<Lang>(ULong) :> Type<Lang>
        | Name "unit" -> return PrimitiveType<Lang>(Void) :> Type<Lang>
        | Name "float"
        | Name "double" ->
            do! State.modify (fun ctx -> ctx.Flags.enableFP64 <- true; ctx)
            return PrimitiveType<Lang>(Double) :> Type<Lang>
        | Name "boolean" ->
            match! State.gets (fun ctx -> ctx.TranslatorOptions |> List.contains UseNativeBooleanType) with
            | true -> return PrimitiveType<Lang>(Bool) :> Type<Lang>
            | false -> return PrimitiveType<Lang>(BoolClAlias) :> Type<Lang>

        | Name "read_only image2D" -> return Image2DType(true) :> Type<Lang>
        | Name "write_only image2D" -> return Image2DType(false) :> Type<Lang>

        | StartsWith "fsharpref" tName ->
            let! translatedType = translate type'.GenericTypeArguments.[0]
            return RefType(translatedType, []) :> Type<Lang>
        | StartsWith "fsharpfunc" tName ->
            return! translate type'.GenericTypeArguments.[1]

        | EndsWith "[]" tName ->
            let! baseT = translate <| type'.GetElementType()
            match! State.gets (fun ctx -> ctx.ArrayKind) with
            | CPointer -> return RefType(baseT, []) :> Type<Lang>
            | CArrayDecl size -> return ArrayType(baseT, size) :> Type<Lang>

        | StartsWith ClArray_ tName
        | StartsWith ClCell_ tName
        | StartsWith IBuffer_ tName ->
            let! baseT = translate type'.GenericTypeArguments.[0]
            match! State.gets (fun ctx -> ctx.ArrayKind) with
            | CPointer -> return RefType(baseT, []) :> Type<Lang>
            | CArrayDecl size -> return ArrayType(baseT, size) :> Type<Lang>

        | other ->
            let! context = State.get
            if context.UserDefinedTypes.Contains other then
                let structType =
                    if context.StructDecls.ContainsKey type' then
                        context.StructDecls.[type']
                    elif context.TupleDecls.ContainsKey type' then
                        context.TupleDecls.[type']
                    elif context.UnionDecls.ContainsKey type' then
                        context.UnionDecls.[type'] :> StructType<_>
                    else
                        failwithf "Declaration of struct %A doesn't exists" other
                return structType :> Type<_>
            else
                return failwithf "Unsupported kernel type: %A" other
    }

    let translateStruct (type': System.Type) = translation {
        let! fields =
            [
                for f in type'.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) ->
                    translate f.PropertyType >>= fun type' ->
                    State.return' { Name = f.Name; Type = type' }
            ]
            @
            [
                if not <| FSharpType.IsRecord type' then
                    for f in type'.GetFields(BindingFlags.Public ||| BindingFlags.Instance) ->
                        translate f.FieldType >>= fun type' ->
                        State.return' { Name = f.Name; Type = type' }
            ]
            |> State.collect

        let fields = fields |> List.distinct

        let! index = State.gets (fun ctx -> ctx.StructDecls.Count)
        let structType = StructType(sprintf "struct%i" index, fields)
        do! State.modify (fun context -> context.StructDecls.Add(type', structType); context)
        return structType
    }

    let translateTuple (type': System.Type) = translation {
        let genericTypeArguments = FSharpType.GetTupleElements type' |> List.ofArray

        let! elements =
            genericTypeArguments
            |> List.mapi
                (fun i type' -> translation {
                    let! translatedType = translate type'
                    return {
                        Name = sprintf "_%i" (i + 1)
                        Type = translatedType
                    }
                })
            |> State.collect

        match! State.gets (fun ctx -> ctx.TupleDecls.ContainsKey type') with
        | true ->
            return! State.gets (fun ctx -> ctx.TupleDecls.[type'])
        | false ->
            let! index = State.gets (fun ctx -> ctx.TupleDecls.Count)
            let tupleDecl = StructType(sprintf "tuple%i" index, elements)
            do! State.modify (fun ctx -> ctx.TupleDecls.Add(type', tupleDecl); ctx)
            return tupleDecl
    }

    let translateUnion (type': System.Type) = translation {
        let name = type'.Name

        let notEmptyCases =
            FSharpType.GetUnionCases type'
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
                                    translate field.PropertyType >>= fun type' ->
                                    State.return' { Name = field.Name; Type = type' }
                            ]
                            |> State.collect

                        return tag, { Name = structName; Type = StructInplaceType(structName + "Type", fields) }
                    }

            ]
            |> State.collect

        let duType = DiscriminatedUnionType(name, fields)
        do! State.modify (fun context -> context.UnionDecls.Add(type', duType); context)

        return duType
    }

    // TODO rename
    let translateSpecificTypes (translate: System.Type -> State<TargetContext,'a>) (types: System.Type list) = translation {
        do! State.modify (fun context -> context.UserDefinedTypes.AddRange types; context)
        return!
            types
            |> List.map
                (fun type' ->
                    translation {
                        let! translatedType = translate type'
                        return StructDecl translatedType
                    }
                )
            |> State.collect
    }
