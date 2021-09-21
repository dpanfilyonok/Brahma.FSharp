namespace Brahma.FSharp

open OpenCL.Net
open System
open System.Runtime.InteropServices

type IClMem =
    abstract member Size: IntPtr
    abstract member Data: obj

// memory flags: https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clCreateBuffer.html

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

type ClMemFlags =
    {
        HostAccessMode: HostAccessMode
        DeviceAccessMode: DeviceAccessMode
        AllocationMode: AllocationMode
    }

    static member Default =
        {
            HostAccessMode = HostAccessMode.ReadWrite
            DeviceAccessMode = DeviceAccessMode.ReadWrite
            AllocationMode = AllocationMode.AllocAndCopyHostPtr
        }

type BufferInitParam<'a> =
    | Data of 'a[]
    | Size of int

type ClBuffer<'a>
    (
        clContext: Context,
        data: BufferInitParam<'a>,
        ?memFlags: ClMemFlags
    ) =

    let memFlags = defaultArg memFlags ClMemFlags.Default

    let elementSize = Marshal.SizeOf typedefof<'a>
    let intPtrSize = IntPtr(Marshal.SizeOf typedefof<IntPtr>)

    let pinnedMemory =
        match data with
        | Data array -> Some <| GCHandle.Alloc(array, GCHandleType.Pinned)
        | _ -> None

    let clMemoryFlags =
        let mutable flags = MemFlags.None

        // match memFlags.HostAccessMode with
        // | HostAccessMode.ReadWrite -> ()
        // | HostAccessMode.ReadOnly -> flags <- flags ||| MemFlags.HostReadOnly
        // | HostAccessMode.WriteOnly -> flags <- flags ||| MemFlags.HostWriteOnly
        // | HostAccessMode.NotAccessible -> flags <- flags ||| MemFlags.HostNoAccess

        match memFlags.DeviceAccessMode with
        | DeviceAccessMode.ReadWrite -> flags <- flags ||| MemFlags.ReadWrite
        | DeviceAccessMode.ReadOnly -> flags <- flags ||| MemFlags.ReadOnly
        | DeviceAccessMode.WriteOnly -> flags <- flags ||| MemFlags.WriteOnly

        match memFlags.AllocationMode with
        | AllocationMode.UseHostPtr -> flags <- flags ||| MemFlags.UseHostPtr
        | AllocationMode.AllocHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr
        | AllocationMode.CopyHostPtr -> flags <- flags ||| MemFlags.CopyHostPtr
        | AllocationMode.AllocAndCopyHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr ||| MemFlags.CopyHostPtr
        // предполагаем, что нам правильно передали флаги (что для нужных параметров буфера нужные флаги)
        //     match data with
        //     | Some x -> flags <- flags ||| MemFlags.CopyHostPtr
        //     | None -> flags <- flags ||| MemFlags.AllocHostPtr

        flags

    let buffer =
        let error = ref Unchecked.defaultof<ErrorCode>
        let (size, data) =
            match data with
            | Data array -> IntPtr(array.Length * elementSize), array :> System.Array
            | Size size -> IntPtr(size * elementSize), null

        let buf = Cl.CreateBuffer(clContext, clMemoryFlags, size, data, error)

        if !error <> ErrorCode.Success then
            raise (Cl.Exception !error)

        buf

    member this.ClMemory = buffer

    member this.Length =
        match data with
        | Data array -> array.Length
        | Size size -> size

    member this.ElementSize = elementSize

    member this.Free() =
        match pinnedMemory with
        | Some x -> x.Free()
        | None -> ()
        buffer.Dispose()

    member this.Dispose() = this.Free()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    interface IClMem with
        member this.Size = intPtrSize
        member this.Data = box buffer

