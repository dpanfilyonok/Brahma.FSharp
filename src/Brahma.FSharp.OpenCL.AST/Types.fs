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
    override this.Children = []

    abstract Size:int
    abstract Matches: obj -> bool

type PrimitiveType<'lang>(pType:PTypes<'lang>) =
    inherit Type<'lang>()

    override this.Size = 32

    override this.Matches(other) =
        match other with
        | :? PrimitiveType<'lang> as o ->
            this.Type.Equals(o.Type)
        | _ -> false

    member this.Type = pType

type ArrayType<'lang>(baseType:Type<'lang>, ?size:int) =
    inherit Type<'lang>()

    override this.Size =
        match size with
        | Some size -> size
        | None -> 0

    member this.BaseType = baseType

    override this.Matches(other) =
        match other with
        | :? ArrayType<'lang> as o ->
            this.BaseType.Matches(o.BaseType)
            // NB: size is omitted in this check
        | _ -> false

type Image2DType<'lang>(modifier:bool) =
    inherit Type<'lang>()

    override this.Size = 32

    override this.Matches(other:obj) =
        match other with
        | :? Image2DType<'lang> as o ->
            this.Equals(o)
            // NB: fields are omitted in this check
        | _ -> false

    member this.Modifier = modifier

//[<Struct>]
type StructField<'lang> =
    { Name: string;
      Type: Type<'lang> }

type StructType<'lang>(name: string, fields: List<StructField<'lang>>)=
    inherit Type<'lang>()

    member this.Fields = fields
    member this.Name = name

    override this.Size =
        this.Fields |> List.sumBy (fun f -> f.Type.Size)

    override this.Matches(other:obj) =
        match other with
        | :? StructType<'lang> as o ->
            this.Name.Equals(o.Name)
            // NB: fields are omitted in this check
        | _ -> false

type StructDecl<'lang>(structType: StructType<'lang>) =
    inherit Node<'lang>()
    interface TopDef<'lang>

    member val StructType : StructType<'lang> = structType with get, set
    override this.Children = []

type TupleType<'lang>(baseStruct: StructType<'lang>, number:int)=
    inherit Type<'lang>()

    override this.Size = baseStruct.Size
    member this.Number = number
    override this.Matches _ = failwith "Not implemented: matches for tuples"

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
