(*** hide ***)
#I "../../src/Brahma.FSharp.OpenCL.Core/bin/Debug/net5.0"
#r "Brahma.FSharp.OpenCL.Core.dll"
#r "Brahma.FSharp.OpenCL.Shared.dll"

open Brahma.FSharp
open FSharp.Quotations

(**
# Data types supported

- Primitive types
- Booleans
- Custom structs
- Records
- Tuples
- Discriminated unions
*)

[<Struct>]
type StructOfIntInt64 =
    val mutable X: int
    val mutable Y: int64
    new(x, y) = { X = x; Y = y }

<@
    fun (range: Range1D) (buffer: ClArray<StructOfIntInt64>) ->
        let gid = range.GlobalID0
        let tmp = buffer.[gid]
        let x = tmp.X
        let y = tmp.Y
        let mutable innerStruct = StructOfIntInt64(x, y)
        innerStruct.X <- x
        innerStruct.Y <- y
        buffer.[gid] <- StructOfIntInt64(innerStruct.X, innerStruct.Y)
@>

(**
# Custom kernels definition
*)

(**
## General rules

Custom kernels can be expressed as lambda functions inside F# quotation.
A lambda doesn't require particular solutions to mark it as a kernel,
but it must take ndrange as a first parameter and return unit.
Moreover, only curried format is supported.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        let gid = range.GlobalID0
        buffer.[gid] <- buffer.[gid] + 10
@>

(**
## Closure

You can also use variables defined outside the kernel lambda itself.
When compiled, they will be converted to constant literals.
*)

let constVal = 10

let parameterizedKernel (op: Expr<int -> int -> int>) =
    <@
        fun (range: Range1D) (buffer: int clarray) ->
            let gid = range.GlobalID0
            buffer.[gid] <- (%op) buffer.[gid] constVal
    @>

(**
## Local funtions

You can also use local funtions inside kernel.
When compiled, they will be converted to OpenCL C functions.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        let gid = range.GlobalID0
        let f x = x + 10
        buffer.[gid] <- f buffer.[gid]
@>

(**
## Generics

Generic kernels also supported.
*)

let genericKernel op =
    <@
        fun (range: Range1D) (buffer: 'a clarray) ->
            let gid = range.GlobalID0
            buffer.[gid] <- op buffer.[gid]
    @>

(**
# Custom kernels language
*)

(**
## Global and local memory space

Use `clarray` and `clcell` types to use data located in global memory inside the kernel.
Use `localArray` and `local` to allocate data in local memory inside the kernel.
*)

<@
    fun (range: Range1D) (globalBuffer: int clarray) ->
        let localBuffer = localArray<int> 10

        let gid = range.GlobalID0
        globalBuffer.[gid] <- globalBuffer.[gid] + 10
@>

(**
## Synchronization barriers

- `barrierLocal` corresponds to `barrier(CLK_LOCAL_MEM_FENCE)`
- `barrierGlobal` corresponds to `barrier(CLK_GLOBAL_MEM_FENCE)`
- `barrierFull` corresponds to `barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE)`
*)

(**
## Atomic functions

Use `atomic` for atomic function call.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic (+) buffer.[0] 10 |> ignore
@>
