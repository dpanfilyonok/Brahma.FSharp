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

module Brahma.FSharp.OpenCL.Printer.Types

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let private printPrimitiveType (pType: PrimitiveType<'lang>) =
    match pType.Type with
    | Bool -> "bool"
    | Char -> "char"
    | UChar -> "uchar"
    | Short -> "short"
    | UShort -> "ushort"
    | Int -> "int"
    | UInt -> "uint"
    | Long -> "long"
    | ULong -> "ulong"
    | Float -> "float"
    | Double -> "double"
    | Half -> "half"
    | Void -> "void"
    | ConstStringLiteral -> "const char *"
    | TypeName tname -> tname
    |> wordL

let rec Print<'lang> (_type: Type<'lang>) =
    match _type with
    | :? (PrimitiveType<'lang>) as pt -> printPrimitiveType pt
    | :? (RefType<'lang>) as rt -> Print rt.BaseType ^^ wordL "*"
    | :? (ArrayType<'lang>) as art -> Print art.BaseType
    | :? (Image2DType<'lang>) as imgt ->
        match imgt.Modifier with
        | true -> wordL "read_only image2D"
        | false -> wordL "write_only image2D"
    | :? (StructInplaceType<'lang>) as s -> PrintStructInplaceType s
    | :? (StructType<'lang>) as s -> wordL s.Name
    | :? (UnionClInplaceType<'lang>) as u -> PrintUnionInplaceType u
    | :? (TupleType<'lang>) as t -> wordL ("tuple" + t.Number.ToString())
    | t -> failwithf "Printer. Unsupported type: %A" t

and PrintAggregatingInplaceType keyword typeName fields =
    let header = [ wordL keyword; wordL typeName ] |> spaceListL

    let body =
        [
            for field in fields ->
                [
                    Print field.Type
                    wordL field.Name
                    wordL ";"
                ]
                |> spaceListL
        ]
        |> aboveListL
        |> braceL

    header ^^ body

and PrintUnionInplaceType (t: UnionClInplaceType<_>) = PrintAggregatingInplaceType "union" t.Name t.Fields

and PrintStructInplaceType (t: StructInplaceType<_>) = PrintAggregatingInplaceType "struct" t.Name t.Fields
