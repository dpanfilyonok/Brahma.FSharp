namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
open FSharp.Reflection
open System.Runtime.CompilerServices

type Alligment() =
    member val Offsets = ResizeArray<int>() with get
    member val Length = 0 with get, set

    override this.ToString() =
        sprintf "%A %i" this.Offsets this.Length

type CustomMarshaler<'a>() =
    let roundUp n x =
        if x % n <> 0 then
            (x / n) * n + n
        else
            x

    let elementAlligment =
        let elementAlligment = Alligment()
        let rec go (type': Type) =
            match type' with
            | _ when type' = typeof<bool> ->
                let size = Marshal.SizeOf typeof<BoolHostAlias>
                let offset = roundUp size elementAlligment.Length
                elementAlligment.Offsets.Add offset
                elementAlligment.Length <- offset + size

            | _ when FSharpType.IsTuple type' ->
                FSharpType.GetTupleElements type' |> Array.iter go

            | _ ->
                let size = Marshal.SizeOf type'
                let offset = roundUp size elementAlligment.Length
                elementAlligment.Offsets.Add offset
                elementAlligment.Length <- offset + size

        go typeof<'a>
        elementAlligment

    member this.ElementTypeSize = elementAlligment.Length

    member this.ElementTypeAlligment = elementAlligment.Offsets

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
                | :? ITuple as tuple ->
                    let typleSize = tuple.Length
                    [ 0 .. typleSize - 1 ] |> List.iter (fun i -> go tuple.[i])

                | _ ->
                    let offset = this.ElementTypeAlligment.[i]
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
            let start = IntPtr.Add(ptr, j * elementAlligment.Length)
            let mutable i = 0
            let rec go (type': Type) =
                match type' with
                | _ when FSharpType.IsTuple type' ->
                    FSharpType.GetTupleElements type'
                    |> Array.map go
                    |> fun x -> FSharpValue.MakeTuple(x, type')

                | _ ->
                    let offset = elementAlligment.Offsets.[i]
                    let x = Marshal.PtrToStructure(IntPtr.Add(start, offset), type')
                    i <- i + 1
                    x

            array.[i] <- unbox<'a> <| go typeof<'a>

