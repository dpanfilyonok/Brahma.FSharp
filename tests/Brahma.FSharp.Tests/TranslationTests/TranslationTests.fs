module TranslationTests

open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator
open Expecto

let translators = [
    FSQuotationToOpenCLTranslator(
        { new IDevice with
            member this.Name = ""
            member this.Platform = Platform.Any
            member this.DeviceType = DeviceType.Default
            member this.MaxWorkGroupSize = 0
            member this.MaxWorkItemDimensions = 0
            member this.MaxWorkItemSizes = [|0|]
            member this.DeviceExtensions = ""
        }
    )
]

let tests = [
    for translator in translators do yield! [
        testList "Tests for translator" <| TranslatorTests.tests translator
        testList "Quotation transformer tests" <| QuotationTransformersTests.tests translator
    ]
]
