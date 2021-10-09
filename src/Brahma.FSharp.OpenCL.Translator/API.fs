namespace Brahma.FSharp.OpenCL.Translator

open Brahma.FSharp.OpenCL.AST

[<AutoOpen>]
module API =
    let [<Literal>] NDRange1D = "range1d"
    let [<Literal>] NDRange2D = "range2d"
    let [<Literal>] NDRange3D = "range3d"

    let [<Literal>] Buffer = "buffer"

    type SpecificBool = byte
    let CSpecificBool = UChar
