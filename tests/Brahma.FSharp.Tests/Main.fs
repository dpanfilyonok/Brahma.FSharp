module ExpectoTemplate

open Expecto
open Brahma.FSharp.OpenCL.Full

[<EntryPoint>]
let main argv =
    Tests.runTests defaultConfig FullTranslatorTests

