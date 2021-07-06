open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.Tests
open Brahma.OpenCL

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv

// [<Tests>]
// let allTests =
//     testList "All tests" [
//         Translator.tests
//         Atomic.tests
//     ]
//     |> testSequenced

// [<EntryPoint>]
// let main argv =
//     allTests
//     |> runTestsWithCLIArgs [] argv

// [<EntryPoint>]
// let main argv =
//     let e = <@ fun (r: _1D) (m: int) -> m <!+ 5 @>

//     // printfn "%A" <| Transformer.quotationTransformer e []
//     printfn "%s" <| Utils.openclCompile e
//     0
