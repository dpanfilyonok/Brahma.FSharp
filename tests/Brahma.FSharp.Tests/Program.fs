open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.Tests
open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape

// TODO format tests
// [<EntryPoint>]
// let main argv =
//     Tests.runTestsInAssembly defaultConfig argv

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

[<EntryPoint>]
let main argv =
    let e =
        <@
            fun (range: _1D) (array: int[]) ->
                let g x = x + 2
                array.[0] <- g 4
        @>

    let a (e: Expr) =
        match e with
        | ShapeLambda (v, expr) -> printfn "yes"
        | Lambdas (v, body) ->
            printfn "%A %A" v body
        | SpecificCall <@ (+) @> (_, t, args) -> printfn "%A" t

    let kernel =
        <@
            fun (range: _1D) (array: int[]) ->
                let gid = range.GlobalID0
                let g x y = atomic add x y
                let a = g array.[gid] 5
                array.[0] <- 1
                // () -- error
        @>

    let command =
        <@ fun (range: _1D) (buf: array<int>) ->
            let localA = localArray<int> 5
            let f (y: int) =
                y + 5

            let b = f localA.[0]
            let a = f buf.[0]

            buf.[0] <- a
        @>

    let command2 =
        <@ fun (range: _1D) (buf: array<int>) ->
            let localA = localArray<int> 5
            let f y =
                let Argi index = if index = 0 then localA.[1] else buf.[2]
                Argi y

            buf.[0] <- f 0
        @>

    printfn "%A" <| Transformer.quotationTransformer kernel []
    printfn "%A" <| Utils.openclTranslate kernel
    // printfn "%A" <| e
    // a e
    0
