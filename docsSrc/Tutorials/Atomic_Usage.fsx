(*** condition: prepare ***)
#I "../../src/Brahma.FSharp.OpenCL.WorkflowBuilder/bin/Debug/net461"
#r "Brahma.FSharp.OpenCL.Extensions.dll"
#r "YC.OpenCL.NET.dll"
#r "YC.Brahma.OpenCL.dll"
#r "YC.Brahma.dll"

open Brahma.OpenCL
open OpenCL.Net

(**
# Atomic Usage
*)

(**
## Target functions
As atomic function you can use
*)

(**
- anonymous lambdas
*)

<@
    fun (range: _1D) (array: int[]) ->
        atomic (+) array.[0] 1 |> ignore
@>

(**
- named lambdas declared inside kernel quotation
*)

<@
    fun (range: _1D) (array: int[]) ->
        let f x y = x + 2 * y
        atomic f array.[0] 1 |> ignore
@>

(**
- quoted lambdas that inserted inside kernel quotation
*)

let f = <@ fun x y -> x + 2 * y @>

<@
    fun (range: _1D) (array: int[]) ->
        atomic %f array.[0] 1 |> ignore
@>

