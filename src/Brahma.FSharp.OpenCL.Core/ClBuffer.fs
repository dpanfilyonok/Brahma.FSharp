namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared

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

    let elementSize =
        if clContext.Translator.TranslatorOptions |> Array.contains UseNativeBooleanType then
            Marshal.SizeOf(typeof<'a>)
        else
            Marshal.SizeOf(if typeof<'a> = typeof<bool> then typeof<BoolHostAlias> else typeof<'a>)

    let intPtrSize = IntPtr(Marshal.SizeOf typedefof<IntPtr>)

    let pinnedMemory =
        match initParam with
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

    let buffer =
        let error = ref Unchecked.defaultof<ErrorCode>
        let (size, data) =
            match initParam with
            | Data array ->
                IntPtr(array.Length * elementSize),
                if clContext.Translator.TranslatorOptions |> Array.contains UseNativeBooleanType then
                    array :> System.Array
                else
                    if typeof<'a> = typeof<bool> then
                        array |> Array.map Convert.ToByte :> System.Array
                    else
                        array :> System.Array
            | Size size ->
                IntPtr(size * elementSize),
                null

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

        member this.ElementSize = elementSize

        member this.Free() =
            match pinnedMemory with
            | Some x -> x.Free()
            | None -> ()

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
