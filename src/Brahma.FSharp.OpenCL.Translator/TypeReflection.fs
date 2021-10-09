module Brahma.FSharp.OpenCL.Translator.TypeReflection

open FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

let hasAttribute<'attr> (tp: System.Type) =
    tp.GetCustomAttributes(false)
    |> Seq.tryFind (fun attr -> attr.GetType() = typeof<'attr>)
    |> Option.isSome

let isStruct = hasAttribute<StructAttribute>

let collectTypes
    expr
    (typePredicate: System.Type -> bool)
    (nestedTypes: System.Type -> array<System.Type>)
    (escapeNames: array<string>)
    =

    let types = Dictionary<System.Type, _>()

    let rec add (t: System.Type) =
        if
            typePredicate t &&
            not <| types.ContainsKey t &&
            not <| Array.exists ((=) t.Name) escapeNames
        then
            nestedTypes t |> Array.iter add
            types.Add(t, ())

    let rec go (e: Expr) =
        add e.Type

        match e with
        | ExprShape.ShapeVar _ -> ()
        | ExprShape.ShapeLambda (_, body) -> go body
        | ExprShape.ShapeCombination (o, l) ->
            o.GetType() |> add
            List.iter go l

    go expr
    types.Keys |> List.ofSeq

let collectStructs expr =
    let escapeNames = [| "Range1D"; "Range2D"; "Range3D" |]

    let nestedTypes (t: System.Type) =
        seq {
            t.GetProperties()
            |> Array.map (fun prop -> prop.PropertyType)

            t.GetFields()
            |> Array.map (fun field -> field.FieldType)
        }
        |> Array.concat

    collectTypes expr isStruct nestedTypes escapeNames

let collectDiscriminatedUnions expr =
    let escapeNames = [||]
    let unionPredicate = FSharpType.IsUnion

    let nestedTypes : System.Type -> array<System.Type> =
        FSharpType.GetUnionCases
        >> Array.map (fun (case: UnionCaseInfo) -> case.GetFields())
        >> Array.concat
        >> Array.map (fun (prop: PropertyInfo) -> prop.PropertyType)

    collectTypes expr unionPredicate nestedTypes escapeNames
