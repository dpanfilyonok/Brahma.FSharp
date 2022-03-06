namespace Brahma.FSharp.OpenCL.Translator

open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open FSharp.Reflection
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Runtime.Serialization

type StructurePacking =
    | StructureElement of {| Size: int; Aligment: int |} * StructurePacking list

    member this.ElementSize = match this with | StructureElement(pack, _) -> pack.Size

type CustomMarshaler() =
    let typePacking = Dictionary<Type, StructurePacking>()

    let typeOffsets = Dictionary<Type, int[]>()

    let blittableTypes =
        Dictionary<Type, bool>(
            dict [
                typeof<decimal>, false
                typeof<byte>, true
                typeof<sbyte>, true
                typeof<int16>, true
                typeof<uint16>, true
                typeof<int32>, true
                typeof<uint32>, true
                typeof<int64>, true
                typeof<uint64>, true
                typeof<nativeint>, true
                typeof<unativeint>, true
                typeof<single>, true
                typeof<double>, true
            ]
        )

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

    let getTypePacking (type': Type) =
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
                    |> List.fold (fun state x -> Utils.roundUp x.Aligment state + x.Size) 0
                    |> Utils.roundUp aligment

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
                    |> List.fold (fun state x -> Utils.roundUp x.Aligment state + x.Size) 0
                    |> Utils.roundUp aligment

                StructureElement({| Size = size; Aligment = aligment |}, elems)

            | UnionType -> failwithf "Union not supported"

            | UserDefinedStuctureType ->
                let elems =
                    type'.GetFields()
                    |> Array.map (fun fi -> fi.FieldType)
                    |> Array.map go
                    |> Array.toList

                let aligment =
                    elems
                    |> List.map (fun (StructureElement(pack, _)) -> pack.Aligment)
                    |> List.max

                let size =
                    elems
                    |> List.map (fun (StructureElement(pack, _)) -> pack)
                    |> List.fold (fun state x -> Utils.roundUp x.Aligment state + x.Size) 0
                    |> Utils.roundUp aligment

                StructureElement({| Size = size; Aligment = aligment |}, elems)

            | PrimitiveType ->
                let size = Marshal.SizeOf (if type' = typeof<bool> then typeof<BoolHostAlias> else type')
                let aligment = size
                StructureElement({| Size = size; Aligment = aligment |}, [])

        go type'

    let getOffsets packing =
        let getFlattenOffsets start packing =
            let offsets = ResizeArray()
            let mutable size = 0

            match packing with
            | StructureElement(_, innerPacking) ->
                for StructureElement(pack, _) in innerPacking do
                    let offset = Utils.roundUp pack.Aligment size
                    offsets.Add (offset + start)
                    size <- offset + pack.Size

                offsets |> Seq.toList

        let rec loop (packing: StructurePacking) (start: int) = seq {
            match packing with
            | StructureElement(_, []) -> start
            | StructureElement(_, innerPacking) ->
                let packingOffsetPairs =
                    getFlattenOffsets start packing
                    |> List.zip innerPacking

                for (packing, offset) in packingOffsetPairs do
                    yield! loop packing offset
        }

        loop packing 0 |> Seq.toArray

    member this.GetTypePacking(type': Type) =
        let mutable packing = Unchecked.defaultof<StructurePacking>
        if typePacking.TryGetValue(type', &packing) then
            packing
        else
            packing <- getTypePacking type'
            typePacking.Add(type', packing)
            packing

    member this.GetTypeOffsets(type': Type) =
        let mutable offsets = Unchecked.defaultof<int[]>
        if typeOffsets.TryGetValue(type', &offsets) then
            offsets
        else
            offsets <- getOffsets <| this.GetTypePacking(type')
            typeOffsets.Add(type', offsets)
            offsets

    member this.IsBlittable(type': Type) =
        let mutable isBlittable = false
        if blittableTypes.TryGetValue(type', &isBlittable) then
            isBlittable
        // TODO is array check useful here?
        elif type'.IsArray then
            let elem = type'.GetElementType()
            isBlittable <- elem.IsValueType && this.IsBlittable(elem)
            blittableTypes.Add(type', isBlittable)
            isBlittable
        else
            try
                let instance = FormatterServices.GetUninitializedObject(type');
                GCHandle.Alloc(instance, GCHandleType.Pinned).Free();
                isBlittable <- true
                // TODO remove code repetition
                blittableTypes.Add(type', isBlittable)
                isBlittable
            with _ ->
                isBlittable <- false
                blittableTypes.Add(type', isBlittable)
                isBlittable

    member this.WriteToUnmanaged(array: 'a[]) =
        let size = array.Length * this.GetTypePacking(typeof<'a>).ElementSize
        let mem = Marshal.AllocHGlobal size
        this.WriteToUnmanaged(array, mem) |> ignore
        size, mem

    member this.WriteToUnmanaged(array: 'a[], ptr: IntPtr) =
        Array.Parallel.iteri (fun j item ->
            let start = IntPtr.Add(ptr, j * this.GetTypePacking(typeof<'a>).ElementSize)
            let mutable i = 0
            let rec go (structure: obj) =
                match structure with
                | Tuple ->
                    let tuple = unbox<ITuple> structure
                    let tupleSize = tuple.Length
                    [ 0 .. tupleSize - 1 ] |> List.iter (fun i -> go tuple.[i])

                | Record ->
                    FSharpValue.GetRecordFields structure
                    |> Array.iter go

                | Union -> failwithf "Union not supported"

                | UserDefinedStucture ->
                    structure.GetType().GetFields()
                    |> Array.map (fun fi -> fi.GetValue(structure))
                    |> Array.iter go

                | Primitive ->
                    let offset = this.GetTypeOffsets(typeof<'a>).[i]
                    let structure =
                        if structure.GetType() = typeof<bool> then
                            box <| Convert.ToByte structure
                        else
                            structure
                    Marshal.StructureToPtr(structure, IntPtr.Add(start, offset), false)
                    i <- i + 1

            go item
        ) array

        array.Length * this.GetTypePacking(typeof<'a>).ElementSize

    member this.ReadFromUnmanaged<'a>(ptr: IntPtr, n: int) =
        let array = Array.zeroCreate<'a> n
        this.ReadFromUnmanaged(ptr, array)
        array

    member this.ReadFromUnmanaged<'a>(ptr: IntPtr, array: 'a[]) =
        Array.Parallel.iteri (fun j _ ->
            let start = IntPtr.Add(ptr, j * this.GetTypePacking(typeof<'a>).ElementSize)
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

                | UserDefinedStuctureType ->
                    let inst = Activator.CreateInstance(type')
                    type'.GetFields()
                    |> Array.map (fun fi -> fi, go fi.FieldType)
                    |> Array.iter (fun (fi, value) -> fi.SetValue(inst, value))

                    inst

                | PrimitiveType ->
                    let offset = this.GetTypeOffsets(typeof<'a>).[i]
                    let structure = Marshal.PtrToStructure(IntPtr.Add(start, offset), (if type' = typeof<bool> then typeof<BoolHostAlias> else type'))
                    let structure =
                        if type' = typeof<bool> then
                            box <| Convert.ToBoolean structure
                        else
                            structure
                    i <- i + 1
                    structure

            array.[j] <- unbox<'a> <| go typeof<'a>
        ) array
