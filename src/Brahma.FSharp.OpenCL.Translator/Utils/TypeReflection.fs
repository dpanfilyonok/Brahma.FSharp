namespace Brahma.FSharp.OpenCL.Translator

open FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System

module TypeReflection =
    let private hasAttribute<'attr> (tp: Type) =
        tp.GetCustomAttributes(false)
        |> Seq.tryFind (fun attr -> attr.GetType() = typeof<'attr>)
        |> Option.isSome

    let collectTypes expr typePredicate (nestedTypes: Type -> Type[]) (escapeNames: string[]) =
        let types = HashSet<Type>()

        let rec add (type': Type) =
            if
                typePredicate type' &&
                not <| types.Contains type' &&
                not <| Array.exists ((=) type'.Name) escapeNames
            then
                nestedTypes type' |> Array.iter add
                types.Add type' |> ignore

        let rec go (expr: Expr) =
            add expr.Type

            match expr with
            | ExprShape.ShapeVar _ -> ()
            | ExprShape.ShapeLambda (_, body) -> go body
            | ExprShape.ShapeCombination (_, exprs) -> List.iter go exprs

        go expr
        types |> List.ofSeq

    let collectUserDefinedStructs expr =
        let isStruct = hasAttribute<StructAttribute>
        let escapeNames = [||]

        let nestedTypes (type': Type) =
            seq {
                type'.GetProperties()
                |> Array.map (fun prop -> prop.PropertyType)

                // dont needed i think
                if not <| FSharpType.IsRecord type' then
                    type'.GetFields()
                    |> Array.map (fun field -> field.FieldType)
            }
            |> Array.concat

        collectTypes expr isStruct nestedTypes escapeNames

    let collectTuples expr =
        let isTuple = FSharpType.IsTuple
        let escapeNames = [||]

        let nestedTypes (type': Type) = FSharpType.GetTupleElements type'

        collectTypes expr isTuple nestedTypes escapeNames

    let collectDiscriminatedUnions expr =
        let unionPredicate = FSharpType.IsUnion
        let escapeNames = [||]

        let nestedTypes (type': Type) =
            FSharpType.GetUnionCases type'
            |> Array.map (fun (case: UnionCaseInfo) -> case.GetFields())
            |> Array.concat
            |> Array.map (fun (prop: PropertyInfo) -> prop.PropertyType)

        collectTypes expr unionPredicate nestedTypes escapeNames
