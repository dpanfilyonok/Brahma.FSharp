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

// returns old
// TODO мб стоит убрать все эти функции кроме xchg, cmpxchg, inc и dec и пользоваться обычными операторами
// let inline add (p: 'a) (value: 'a) : 'a = p + value
// let inline sub (p: 'a) (value: 'a) = p - value
let inline inc (p: 'a) = p + p
let inline dec (p: 'a) = p - p
let inline xchg (p: 'a) (value: 'a) = p
let inline cmpxchg (p: 'a) (cmp: 'a) (value: 'a) = if p = cmp then value else p
// let inline min (p: 'a) (value: 'a) = min p value
// let inline max (p: 'a) (value: 'a) = max p value
// let inline and' (p: 'a) (value: 'a) = p &&& value
// let inline or' (p: 'a) (value: 'a) = p ||| value
// let inline xor (p: 'a) (value: 'a) = p ^^^ value

// let _byte (x: bool) = 0uy

// let as_uint (b1: byte) (b2: byte) (b3: byte) (b4: byte) = uint32 1


/// Alias for atom_add. Not returns old value in F#.
/// ### Example
/// a.[i] <!+ buf
let inline (<!+) a b =
    kernelFail ()
    a + b |> ignore

/// Alias for atom_add. Returns old value.
/// ### Example
/// let oldV = a.[i] <!+> buf
let inline (<!+>) a b =
    kernelFail ()
    a + b

/// Alias for atom_sub. Not returns old value in F#.
/// ### Example
/// a.[i] <!- buf
let inline (<!-) a b =
    kernelFail ()
    a - b |> ignore

/// Alias for atom_sub. Returns old value.
/// ### Example
/// let oldV = a.[i] <!-> buf
let inline (<!->) a b =
    kernelFail ()
    a - b

/// Alias for atom_xchg. Not returns old value in F#
/// ### Example
/// a.[i] <! buf
let inline (<!) (a: 'a) (b: 'a) =
    kernelFail ()
    b |> ignore

/// Alias for atom_xchg. Returns old value.
/// ### Example
/// let oldV = a.[i] <!> buf
let inline (<!>) (a: 'a) (b: 'a) =
    kernelFail ()
    b

//let (<&&>) (a:uint16) b =
//    kernelFail ()
//    a &&& b
//
//let (<&&>) (a:int) b =
//    kernelFail ()
//    a &&& b
let aIncrR a =
    kernelFail ()
    a + 1

let aIncr a =
    kernelFail ()
    a + 1 |> ignore

let aDecr a =
    kernelFail ()
    a - 1 |> ignore

let aDecrR a =
    kernelFail ()
    a - 1

let aMax a b =
    kernelFail ()
    max a b |> ignore

let aMaxR a b =
    kernelFail ()
    max a b

let aMin a b =
    kernelFail ()
    min a b |> ignore

let aMinR a b =
    kernelFail ()
    min a b

let aCompExch a b c =
    kernelFail ()
    if a = b then c else a |> ignore

let aCompExchR a b c =
    kernelFail ()
    if a = b then c else a |> ignore
    a
