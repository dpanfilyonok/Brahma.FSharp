namespace Brahma.FSharp.OpenCL.Translator

open FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System

module TypeReflection =
    let hasAttribute<'attr> (tp: Type) =
        tp.GetCustomAttributes(false)
        |> Seq.tryFind (fun attr -> attr.GetType() = typeof<'attr>)
        |> Option.isSome

    let isStruct = hasAttribute<StructAttribute>

    let collectTypes
        expr
        (typePredicate: Type -> bool)
        (nestedTypes: Type -> Type[])
        (escapeNames: string[])
        =

        // TODO dict (type => unit) for what?
        let types = Dictionary<Type, _>()

        let rec add (t: Type) =
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
                // nestedTypes (o.GetType()) |> Array.iter add
                // o.GetType() |>  typePredicate |> printfn "%A"
                List.iter go l

        go expr
        types.Keys |> List.ofSeq

    let collectUserDefinedStructs expr =
        let escapeNames = [||]

        let nestedTypes (t: Type) =
            seq {
                t.GetProperties()
                |> Array.map (fun prop -> prop.PropertyType)

                t.GetFields()
                |> Array.map (fun field -> field.FieldType)
            }
            |> Array.concat

        collectTypes expr isStruct nestedTypes escapeNames

    // TODO fix it
    let collectDiscriminatedUnions expr =
        let escapeNames = [||]
        let unionPredicate = FSharpType.IsUnion

        let nestedTypes : Type -> Type[] =
            FSharpType.GetUnionCases
            >> Array.map (fun (case: UnionCaseInfo) -> case.GetFields())
            >> Array.concat
            >> Array.map (fun (prop: PropertyInfo) -> prop.PropertyType)

        collectTypes expr unionPredicate nestedTypes escapeNames

    let collectTuples expr =
        let escapeNames = [||]
        let isTuple = FSharpType.IsTuple
        collectTypes expr isTuple FSharpType.GetTupleElements escapeNames
