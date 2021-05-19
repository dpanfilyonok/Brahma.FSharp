module ExpectoTemplate

open Expecto
open Brahma.FSharp.OpenCL.Full
open Brahma.FSharp.Tests.QuotationTransformer

[<EntryPoint>]
let main argv =
    let tests =
        testList "tests"
            [
                FullTranslatorTests
                quotationTransformerTests
            ]

    Tests.runTests defaultConfig tests
