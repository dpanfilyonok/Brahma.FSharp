[<AutoOpen>]
module OpenCL

let kernelFail () =
    failwith "Seems that you try to use openCL kernel function as regular F# function!"

let barrier () = ignore null

let local<'a when 'a: struct> () =
    kernelFail ()
    Unchecked.defaultof<'a>

let localArray<'a> (size: int) =
    kernelFail ()
    Unchecked.defaultof<array<'a>>

let atomic (f: 'a -> 'b) =
    kernelFail ()
    f

let inline inc (p: 'a) = p + p
let inline dec (p: 'a) = p - p
let inline xchg (p: 'a) (value: 'a) = p
let inline cmpxchg (p: 'a) (cmp: 'a) (value: 'a) = if p = cmp then value else p

// let _byte (x: bool) = 0uy
// let as_uint (b1: byte) (b2: byte) (b3: byte) (b4: byte) = uint32 1
