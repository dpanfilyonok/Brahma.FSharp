namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator

//memory flags: https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clCreateBuffer.html

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
            AllocationMode = AllocationMode.AllocHostPtr
        }

type BufferInitParam<'a> =
    | Data of 'a[]
    | Size of int

type ClBuffer<'a when 'a : struct>
    (
        provider: ComputeProvider,
        data: BufferInitParam<'a>,
        ?memFlags: ClMemFlags
    ) =

    let memFlags = defaultArg memFlags ClMemFlags.Default

    let elementSize =
        if provider.Translator.TranslatorOptions |> Array.contains UseNativeBooleanType then
            Marshal.SizeOf(typeof<'a>)
        else
            Marshal.SizeOf(if typeof<'a> = typeof<bool> then typeof<SpecificBool> else typeof<'a>)

    let intPtrSize = IntPtr(Marshal.SizeOf typedefof<IntPtr>)

    let pinnedMemory =
        match data with
        | Data array -> Some <| GCHandle.Alloc(array, GCHandleType.Pinned)
        | _ -> None

    let clMemoryFlags =
        let mutable flags = MemFlags.None

        match memFlags.HostAccessMode with
        | HostAccessMode.ReadWrite -> ()
        | HostAccessMode.ReadOnly -> flags <- flags ||| MemFlags.HostReadOnly
        | HostAccessMode.WriteOnly -> flags <- flags ||| MemFlags.HostWriteOnly
        | HostAccessMode.NotAccessible -> flags <- flags ||| MemFlags.HostNoAccess

        match memFlags.DeviceAccessMode with
        | DeviceAccessMode.ReadWrite -> flags <- flags ||| MemFlags.ReadWrite
        | DeviceAccessMode.ReadOnly -> flags <- flags ||| MemFlags.ReadOnly
        | DeviceAccessMode.WriteOnly -> flags <- flags ||| MemFlags.WriteOnly

        match memFlags.AllocationMode with
        | AllocationMode.UseHostPtr -> flags <- flags ||| MemFlags.UseHostPtr
        | AllocationMode.AllocHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr
        | AllocationMode.CopyHostPtr -> flags <- flags ||| MemFlags.CopyHostPtr
        | AllocationMode.AllocAndCopyHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr ||| MemFlags.CopyHostPtr

        // TODO fix flags
        // match data with
        // | Size _ -> () //flags <- flags ||| MemFlags.AllocHostPtr // ????
        // | Data _ -> flags <- flags ||| MemFlags.CopyHostPtr

        flags

    let buffer =
        let error = ref Unchecked.defaultof<ErrorCode>
        let (size, data) =
            match data with
            | Data array ->
                IntPtr(array.Length * elementSize),
                if provider.Translator.TranslatorOptions |> Array.contains UseNativeBooleanType then
                    array :> System.Array
                else
                    if typeof<'a> = typeof<bool> then
                        array |> Array.map Convert.ToByte :> System.Array
                    else
                        array :> System.Array
            | Size size ->
                IntPtr(size * elementSize),
                null

        let buf = Cl.CreateBuffer(provider.ClContext, clMemoryFlags, size, data, error)

        if !error <> ErrorCode.Success then
            raise <| Cl.Exception !error

        buf

    interface IBuffer<'a> with
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

    interface IDisposable with
        member this.Dispose() = (this :> IBuffer<'a>).Free()

    interface IClMem with
        member this.Size = intPtrSize
        member this.Data = box buffer

    member this.Dispose() = (this :> IDisposable).Dispose()
    member this.Length = (this :> IBuffer<'a>).Length
