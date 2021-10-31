namespace Brahma.FSharp.OpenCL.Translator

open Brahma.FSharp.OpenCL.AST

[<AutoOpen>]
module Bindings =
    let [<Literal>] NDRange1D = "range1d"
    let [<Literal>] NDRange2D = "range2d"
    let [<Literal>] NDRange3D = "range3d"

    let [<Literal>] ClArray = "clarray"
    let [<Literal>] ClCell = "clcell"

    type BoolHostAlias = byte
    let BoolClAlias = UChar
