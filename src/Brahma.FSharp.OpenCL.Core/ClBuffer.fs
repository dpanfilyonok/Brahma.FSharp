namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
open FSharp.Reflection
open System.Runtime.CompilerServices

//memory flags: https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clCreateBuffer.html

exception InvalidMemFlagsException of string

[<RequireQualifiedAccess>]
type HostAccessMode =
    | ReadWrite
    | ReadOnly
    | WriteOnly
    | NotAccessible

[<RequireQualifiedAccess>]
type DeviceAccessMode =
    | ReadWrite
    | ReadOnly
    | WriteOnly

[<RequireQualifiedAccess>]
type AllocationMode =
    | UseHostPtr
    | AllocHostPtr
    | CopyHostPtr
    | AllocAndCopyHostPtr
    | Default

type ClMemFlags =
    {
        HostAccessMode: HostAccessMode
        DeviceAccessMode: DeviceAccessMode
        AllocationMode: AllocationMode
    }

    static member DefaultIfData =
        {
            HostAccessMode = HostAccessMode.ReadWrite
            DeviceAccessMode = DeviceAccessMode.ReadWrite
            AllocationMode = AllocationMode.AllocAndCopyHostPtr
        }

    static member DefaultIfNoData =
        {
            HostAccessMode = HostAccessMode.ReadWrite
            DeviceAccessMode = DeviceAccessMode.ReadWrite
            AllocationMode = AllocationMode.AllocHostPtr
        }

type Alligment() =
    member val Offsets = ResizeArray<int>() with get
    member val Length = 0 with get, set

    override this.ToString() =
        sprintf "%A %i" this.Offsets this.Length

type BufferInitParam<'a> =
    | Data of 'a[]
    | Size of int

type ClBuffer<'a when 'a : struct>
    (
        clContext: IContext,
        initParam: BufferInitParam<'a>,
        ?memFlags: ClMemFlags
    ) =

    let memFlags =
        match initParam with
        | Data _ -> ClMemFlags.DefaultIfData
        | Size _ -> ClMemFlags.DefaultIfNoData
        |> defaultArg memFlags

    let roundUp n x =
        if x % n <> 0 then
            (x / n) * n + n
        else
            x

    let getElementSize (type': Type) =
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

        go type'
        elementAlligment

    let intPtrSize = IntPtr(Marshal.SizeOf typedefof<IntPtr>)
    let elementAlligment = getElementSize typeof<'a>

    // let pinnedMemory =
    //     match initParam with
    //     | Data array -> Some <| GCHandle.Alloc(array, GCHandleType.Pinned)
    //     | _ -> None

    let clMemoryFlags =
        let mutable flags = MemFlags.None

        match memFlags.HostAccessMode with
        | HostAccessMode.ReadWrite -> ()
        | HostAccessMode.ReadOnly -> flags <- flags ||| MemFlags.HostReadOnly
        | HostAccessMode.WriteOnly -> flags <- flags ||| MemFlags.HostWriteOnly
        | HostAccessMode.NotAccessible -> flags <- flags ||| MemFlags.HostNoAccess

        match memFlags.DeviceAccessMode with
        | DeviceAccessMode.ReadWrite -> flags <- flags ||| MemFlags.ReadWrite // default
        | DeviceAccessMode.ReadOnly -> flags <- flags ||| MemFlags.ReadOnly
        | DeviceAccessMode.WriteOnly -> flags <- flags ||| MemFlags.WriteOnly

        let ifDataFlags = [
            AllocationMode.UseHostPtr
            AllocationMode.CopyHostPtr
            AllocationMode.AllocAndCopyHostPtr
        ]

        match initParam with
        | Size _  when ifDataFlags |> List.contains memFlags.AllocationMode ->
            raise <| InvalidMemFlagsException(sprintf "One of following flags should be setted %O" ifDataFlags)
        | Data _ when ifDataFlags |> List.contains memFlags.AllocationMode |> not ->
            raise <| InvalidMemFlagsException(sprintf "Neither of following flags should be setted %O" ifDataFlags)
        | _ -> ()

        match memFlags.AllocationMode with
        | AllocationMode.UseHostPtr -> flags <- flags ||| MemFlags.UseHostPtr
        | AllocationMode.AllocHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr
        | AllocationMode.CopyHostPtr -> flags <- flags ||| MemFlags.CopyHostPtr
        | AllocationMode.AllocAndCopyHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr ||| MemFlags.CopyHostPtr
        | AllocationMode.Default -> ()

        flags

    // NOTE булы на девайсе вообще не поддерживаются в Opencl
    // NOTE нужно ли поддерживать вектора

    do printfn "%O" elementAlligment

    let buffer =
        let error = ref Unchecked.defaultof<ErrorCode>
        let (size, data) =
            match initParam with
            | Data array ->
                let size = array.Length * elementAlligment.Length
                let mem = Marshal.AllocHGlobal size
                for k = 0 to array.Length - 1 do
                    let start = IntPtr.Add(mem, k * elementAlligment.Length)
                    let mutable i = 0
                    let rec go (structure: obj) =
                        match structure with
                        | :? bool ->
                            let offset = elementAlligment.Offsets.[i]
                            Marshal.StructureToPtr(structure, IntPtr.Add(start, offset), false)
                            i <- i + 1

                        | :? ITuple as tuple ->
                            let typleSize = tuple.Length
                            [ 0 .. typleSize - 1 ] |> List.iter (fun i -> go tuple.[i])

                        | _ ->
                            let offset = elementAlligment.Offsets.[i]
                            Marshal.StructureToPtr(structure, IntPtr.Add(start, offset), false)
                            i <- i + 1

                    go array.[k]

                IntPtr size, mem

            // TODO null return
            | Size size ->
                IntPtr(size * elementAlligment.Length),
                Marshal.AllocHGlobal (size * elementAlligment.Length)

        let buf = Cl.CreateBuffer(clContext.Context, clMemoryFlags, size, data, error)

        if !error <> ErrorCode.Success then
            raise <| Cl.Exception !error

        buf

    member this.ClContext = clContext

    interface IBuffer<'a> with
        member this.Memory = buffer

        member this.Length =
            match initParam with
            | Data array -> array.Length
            | Size size -> size

        member this.ElementSize = elementAlligment.Length

        member this.Free() =
            // match pinnedMemory with
            // | Some x -> x.Free()
            // | None -> ()

            buffer.Dispose()

        member this.Item
            with get (idx: int) : 'a = FailIfOutsideKernel()
            and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    interface IDisposable with
        member this.Dispose() = (this :> IBuffer<'a>).Free()

    interface IClMem with
        member this.Size = intPtrSize
        member this.Data = box buffer

    member this.Dispose() = (this :> IDisposable).Dispose()
    member this.Length = (this :> IBuffer<'a>).Length
