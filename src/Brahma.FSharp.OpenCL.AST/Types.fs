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

namespace Brahma.FSharp.OpenCL.AST

type PTypes<'lang> =
    | Bool
    | Char
    | UChar
    | Short
    | UShort
    | Int
    | UInt
    | Long
    | ULong
    | Float
    | Double
    | Half
    | Void
    | TypeName of string

[<AbstractClass>]
type Type<'lang>()=
    inherit Node<'lang>()
    abstract Size:int
    abstract Matches: obj -> bool

type PrimitiveType<'lang>(pType:PTypes<'lang>) =
    inherit Type<'lang>()
    override this.Size = 32
    override this.Children = []
    member this.Type = pType

    override this.Matches(other) =
        match other with
        | :? PrimitiveType<'lang> as o ->
            this.Type.Equals(o.Type)
        | _ -> false

type ArrayType<'lang>(baseType:Type<'lang>, ?size:int) =
    inherit Type<'lang>()
    override this.Size =
        match size with
        | Some size -> size
        | None -> 0
    override this.Children = []
    member this.BaseType = baseType

    override this.Matches(other) =
        match other with
        | :? ArrayType<'lang> as o ->
            this.BaseType.Matches(o.BaseType)
            // NB: size is omitted in this check
        | _ -> false

[<Struct>]
type StructField<'lang> =
    val FName: string
    val FType: Type<'lang>

    new (fName, fType) = {FName = fName; FType = fType}

type Struct<'lang>(name: string, fields: List<StructField<'lang>>) =
    inherit TopDef<'lang>()
    override this.Children = []
    member this.Fields = fields
    member this.Name = name
    member this.Matches(other:obj) =
        match other with
        | :? Struct<'lang> as o ->
            this.Name.Equals(o.Name)
            // NB: fields are omitted in this check
        | _ -> false

type StructType<'lang>(decl)=
    inherit Type<'lang>()
    member val Declaration : Option<Struct<'lang>> = decl with get, set
    override this.Children = []
    override this.Size =
        match  this.Declaration with
        | Some decl -> decl.Fields |> List.sumBy (fun f -> f.FType.Size)
        | None -> 0

    override this.Matches(other) =
        match other with
        | :? StructType<'lang> as o ->
            match this.Declaration, o.Declaration with
            | Some x, Some y -> x.Matches(y)
            | None, None -> true
            | _ -> false
            // NB: size is omitted in this check
        | _ -> false

type RefType<'lang>(baseType:Type<'lang>, typeQuals:TypeQualifier<'lang> list) =
    inherit Type<'lang>()
    override this.Size = baseType.Size
    override this.Children = []
    member this.BaseType = baseType
    member this.TypeQuals = typeQuals

    override this.Matches(other) =
        match other with
        | :? RefType<'lang> as o ->
            this.BaseType.Matches(o.BaseType)
            && this.TypeQuals.Equals(o.TypeQuals)
        | _ -> false
