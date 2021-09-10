namespace Brahma.FSharp.OpenCL

open OpenCL.Net

type IClMem =
    abstract member Size : System.IntPtr
    abstract member Data : obj

//memory flags: https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clCreateBuffer.html

[<RequireQualifiedAccess>]
type DeviceAccessMode =
    | ReadWrite
    | ReadOnly
    | WriteOnly

[<RequireQualifiedAccess>]
type HostAccessMode =
    | ReadWrite
    | ReadOnly
    | WriteOnly
    | NotAccessible

[<RequireQualifiedAccess>]
type AllocationMode =
    | UseHostPtr
    | AllocHostPtr
    | CopyHostPtr
    | AllocAndCopyHostPtr

type Buffer<'t> private (clContext: OpenCL.Net.Context, length:int, data:option<array<'t>>, hostAccessMode, allocationMode, deviceAccessMode) =
    
    let elementSize = System.Runtime.InteropServices.Marshal.SizeOf(typedefof<'t>)
    let intPtrSize = System.IntPtr(System.Runtime.InteropServices.Marshal.SizeOf(typedefof<System.IntPtr>))
   
    let pinnedMemory = 
        match data with
        | None -> None
        | Some x -> 
                System.Runtime.InteropServices.GCHandle.Alloc(x, System.Runtime.InteropServices.GCHandleType.Pinned)
                |> Some

    let clMemoryFlags = 
        let mutable flags = MemFlags.None
        match hostAccessMode with
        | None -> ()
        | Some x -> match x with 
                    | HostAccessMode.ReadWrite -> ()
                    | HostAccessMode.ReadOnly -> flags <- flags ||| MemFlags.HostReadOnly
                    | HostAccessMode.WriteOnly -> flags <- flags ||| MemFlags.HostWriteOnly
                    | HostAccessMode.NotAccessible -> flags <- flags ||| MemFlags.HostNoAccess

        match allocationMode with
        | None -> 
            match data with 
            | None -> flags <- flags ||| MemFlags.AllocHostPtr
            | Some x -> flags <- flags ||| MemFlags.CopyHostPtr
        | Some x -> match x with 
                    | AllocationMode.UseHostPtr -> flags <- flags ||| MemFlags.UseHostPtr
                    | AllocationMode.AllocHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr
                    | AllocationMode.CopyHostPtr -> flags <- flags ||| MemFlags.CopyHostPtr
                    | AllocationMode.AllocAndCopyHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr ||| MemFlags.CopyHostPtr

        match deviceAccessMode with
        | None -> flags <- flags ||| MemFlags.ReadWrite
        | Some x -> match x with 
                    | DeviceAccessMode.ReadWrite -> flags <- flags ||| MemFlags.ReadWrite
                    | DeviceAccessMode.ReadOnly -> flags <- flags ||| MemFlags.ReadOnly
                    | DeviceAccessMode.WriteOnly -> flags <- flags ||| MemFlags.WriteOnly                    

        flags

    let buffer = 
        let error = ref Unchecked.defaultof<ErrorCode>
        let size = System.IntPtr(length * elementSize)
        let data = 
            match data with 
            | None ->  null
            | Some data -> data :> System.Array
        
        let buf = Cl.CreateBuffer(clContext, clMemoryFlags, size, data, error)
            
        if !error <> ErrorCode.Success
        then raise (Cl.Exception !error)
        
        buf

    member this.ClMemory = buffer
    member this.Length = length
    member this.ElementSize = elementSize
    
    member this.Item
        with get index = 
            failwith "Kernel only."
            Unchecked.defaultof<'t>

        and set index value =
            failwith "Kernel only."
            ()


    member this.Free() = 
        match pinnedMemory with 
        | None -> ()
        | Some x -> x.Free()
        buffer.Dispose()


    interface System.IDisposable with
        member this.Dispose() = this.Free()

    interface IClMem with
        member this.Size = intPtrSize
        member this.Data = box buffer

    new (clContext: OpenCL.Net.Context, data:array<'t>, ?hostAccessMode:HostAccessMode, ?allocationMode:AllocationMode, ?deviceAccessMode:DeviceAccessMode) =        
        new Buffer<_>(clContext, data.Length, Some data, hostAccessMode, allocationMode, deviceAccessMode)

    new (clContext: OpenCL.Net.Context, length:int, ?hostAccessMode:HostAccessMode, ?allocationMode:AllocationMode, ?deviceAccessMode:DeviceAccessMode) =        
        new Buffer<_>(clContext, length, None, hostAccessMode, allocationMode, deviceAccessMode)