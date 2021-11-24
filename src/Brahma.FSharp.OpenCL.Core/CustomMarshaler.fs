namespace Brahma.FSharp.OpenCL

open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open FSharp.Reflection
open System.Runtime.CompilerServices

type StructurePacking =
    | StructureElement of {| Size: int; Aligment: int |} * StructurePacking list

module private Utils =
    let hasAttribute<'attr> (tp: Type) =
        tp.GetCustomAttributes(false)
        |> Seq.tryFind (fun attr -> attr.GetType() = typeof<'attr>)
        |> Option.isSome

type CustomMarshaler<'a>() =
    let (|TupleType|RecordType|UnionType|UserDefinedStuctureType|PrimitiveType|) (type': Type) =
        match type' with
        | _ when FSharpType.IsTuple type' -> TupleType
        | _ when FSharpType.IsRecord type' -> RecordType
        | _ when FSharpType.IsUnion type' -> UnionType
        | _ when Utils.hasAttribute<StructAttribute> type' -> UserDefinedStuctureType
        | _ -> PrimitiveType

    let (|Tuple|Record|Union|UserDefinedStucture|Primitive|) (structure: obj) =
        match structure.GetType() with
        | TupleType -> Tuple
        | RecordType -> Record
        | UnionType -> Union
        | UserDefinedStuctureType -> UserDefinedStucture
        | _ -> Primitive

    let roundUp n x =
        if x % n <> 0 then
            (x / n) * n + n
        else
            x

    let elementPacking =
        let rec go (type': Type) =
            match type' with
            | TupleType ->
                let elems =
                    FSharpType.GetTupleElements type'
                    |> Array.map go
                    |> Array.toList

                let aligment =
                    elems
                    |> List.map (fun (StructureElement(pack, _)) -> pack.Aligment)
                    |> List.max

                let size =
                    elems
                    |> List.map (fun (StructureElement(pack, _)) -> pack)
                    |> List.fold (fun state x -> roundUp x.Aligment state + x.Size) 0
                    |> roundUp aligment

                StructureElement({| Size = size; Aligment = aligment |}, elems)

            | RecordType ->
                let elems =
                    FSharpType.GetRecordFields type'
                    |> Array.map (fun pi -> pi.PropertyType)
                    |> Array.map go
                    |> Array.toList

                let aligment =
                    elems
                    |> List.map (fun (StructureElement(pack, _)) -> pack.Aligment)
                    |> List.max

                let size =
                    elems
                    |> List.map (fun (StructureElement(pack, _)) -> pack)
                    |> List.fold (fun state x -> roundUp x.Aligment state + x.Size) 0
                    |> roundUp aligment

                StructureElement({| Size = size; Aligment = aligment |}, elems)

            | UnionType -> failwithf "Union not supported"
            | UserDefinedStuctureType -> failwithf "Custom structures not supported"

            | PrimitiveType ->
                let size = Marshal.SizeOf (if type' = typeof<bool> then typeof<BoolHostAlias> else type')
                let aligment = size
                StructureElement({| Size = size; Aligment = aligment |}, [])

        go typeof<'a>

    let flattenOffsets start packing =
        let offsets = ResizeArray()
        let mutable size = 0

        match packing with
        | StructureElement(_, innerPacking) ->
            for StructureElement(pack, _) in innerPacking do
                let offset = roundUp pack.Aligment size
                offsets.Add (offset + start)
                size <- offset + pack.Size

            offsets |> Seq.toList

    let offsets =
        let rec loop (packing: StructurePacking) (start: int) = seq {
            match packing with
            | StructureElement(_, []) -> start
            | StructureElement(_, innerPacking) ->
                let packingOffsetPairs =
                    flattenOffsets start packing
                    |> List.zip innerPacking

                for (packing, offset) in packingOffsetPairs do
                    yield! loop packing offset
        }

        loop elementPacking 0

    member this.ElementTypeSize = match elementPacking with | StructureElement(pack, _) -> pack.Size

    member this.ElementTypeOffsets = offsets |> Seq.toArray

    member this.WriteToUnmanaged(array: 'a[]) =
        let size = array.Length * this.ElementTypeSize
        let mem = Marshal.AllocHGlobal size
        this.WriteToUnmanaged(array, mem) |> ignore
        size, mem

    member this.WriteToUnmanaged(array: 'a[], ptr: IntPtr) =
        for j = 0 to array.Length - 1 do
            let start = IntPtr.Add(ptr, j * this.ElementTypeSize)
            let mutable i = 0
            let rec go (structure: obj) =
                match structure with
                | Tuple ->
                    let tuple = unbox<ITuple> structure
                    let tupleSize = tuple.Length
                    [ 0 .. tupleSize - 1 ] |> List.iter (fun i -> go tuple.[i])

                | Record ->
                    FSharpValue.GetRecordFields structure |> Array.iter go

                | Union -> failwithf "Union not supported"
                | UserDefinedStucture -> failwithf "Custom structures not supported"

                | Primitive ->
                    let offset = this.ElementTypeOffsets.[i]
                    let structure =
                        if structure.GetType() = typeof<bool> then
                            box <| Convert.ToByte structure
                        else
                            structure
                    Marshal.StructureToPtr(structure, IntPtr.Add(start, offset), false)
                    i <- i + 1

            go array.[j]

        array.Length * this.ElementTypeSize

    member this.ReadFromUnmanaged(ptr: IntPtr, n: int) =
        let array = Array.zeroCreate<'a> n
        this.ReadFromUnmanaged(ptr, array)
        array

    member this.ReadFromUnmanaged(ptr: IntPtr, array: 'a[]) =
        for j = 0 to array.Length - 1 do
            let start = IntPtr.Add(ptr, j * this.ElementTypeSize)
            let mutable i = 0
            let rec go (type': Type) =
                match type' with
                | TupleType ->
                    FSharpType.GetTupleElements type'
                    |> Array.map go
                    |> fun x -> FSharpValue.MakeTuple(x, type')

                | RecordType ->
                    FSharpType.GetRecordFields type'
                    |> Array.map (fun pi -> pi.PropertyType)
                    |> Array.map go
                    |> fun x -> FSharpValue.MakeRecord(type', x)

                | UnionType -> failwithf "Union not supported"
                | UserDefinedStuctureType -> failwithf "Custom structures not supported"

                | PrimitiveType ->
                    let offset = this.ElementTypeOffsets.[i]
                    let structure = Marshal.PtrToStructure(IntPtr.Add(start, offset), (if type' = typeof<bool> then typeof<BoolHostAlias> else type'))
                    let structure =
                        if type' = typeof<bool> then
                            box <| Convert.ToBoolean structure
                        else
                            structure
                    i <- i + 1
                    structure

            array.[j] <- unbox<'a> <| go typeof<'a>

    override this.ToString() =
        sprintf "%O\n%A" elementPacking offsets
