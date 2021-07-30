open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Translator.QuotationsTransformer
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.Tests
open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Core.LanguagePrimitives
open Brahma.FSharp.OpenCL.WorkflowBuilder
open Brahma.FSharp.OpenCL.Extensions
open Brahma.OpenCL
open OpenCL.Net

// [<Tests>]
// let allTests =
//     testList "All tests" [
//         Translator.tests
//         Full.tests
//         Atomic.tests
//         Workflow.tests
//         QuotationTransformer.tests
//         Union.tests
//     ]

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
        | ShapeLambda (v, expr) -> printfn "no"
        | Lambdas (v, body) ->
            printfn "%A %A" v body
        | SpecificCall <@ IntrinsicFunctions.GetArray @> (_, t, args) -> printfn "yes"

    let kernel =
        <@
            fun (range: _1D) (array: int[]) ->
                // TODO что-то не то с тем, что парамет атомарной функции должнен быть указателем
                let g x y = atomic (/) x y
                let a = g array.[0] 5
                array.[0] <- 1
                // () -- error
        @>

    let kernel2 =
        <@
            fun (range: _1D) (array: int[]) ->
                // NOTE тут все норм
                let a = atomic (/) array.[0] 5
                array.[0] <- 1
                // () -- error
        @>

    let kernel2 =
        <@
            fun (range: _1D) (array: int[]) ->
                // NOTE тут все норм
                let a =
                    let f x y = (/) x y
                    let g x y = atomic f x y
                    g array.[0] 5
                array.[0] <- 1
                // () -- error
        @>

    let kernel3 =
        <@
            fun (range: _1D) (array: int[]) ->
                while atomic (+) array.[0] 6 = 7 do
                    array.[0] <- 1
                array.[0] <- 1
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

    let k4 =
        <@
            // не работает, тк у лямбды больше аргументов
            fun (range: _1D) (buf: array<int>) ->
                let a = atomic (fun x y -> x + 1) buf.[0]
                buf.[0] <- 0
        @>

    let k4 =
        <@
            fun (range: _1D) (buf: array<int>) ->
                let a = atomic (fun x -> x / 2) buf.[0]
                buf.[0] <- 0
        @>

    // не в модуле, поэтому application, иначе call
    let s =
        <@
            fun (range: _1D) (buf: array<int>) ->
                let old =
                    let f x = x + 1
                    1 + 2
                // let f x y = x + y
                // let a = f buf.[0] 5
                buf.[0] <- 0
        @>

    let s =
        <@
            fun (range: _1D) (buf: array<int>) ->
                // let mutable a = 1
                let old =
                    let f x = x + 1
                // a <- 0
                    buf.[0] <- 0
                // let f x y = x + y
                // let a = f buf.[0] 5
                buf.[0] <- 0
        @>

    let s =
        <@
            fun (range: _1D) (buf: array<int>) (q: int) ->
                // let mutable a = 1
                let f x = x + 1
                let a = local<int> ()
                let b = f a
                // a <- 0
                buf.[0] <- 0
                // let f x y = x + y
                // let a = f buf.[0] 5
        @>

    printfn "%A" <| k4
    printfn "%A" <| ProcessAtomic.processAtomic k4
    printfn "%A" <| Transformer.quotationTransformer k4 []
    printfn "%A" <| Utils.openclTranslate k4
    // printfn "%A" <| a <@ [|1|].[0] @>
    // a e

    // let kernel =
    //     <@
    //         fun (range: _1D) (mutexArray: int[]) (buffer: int[]) ->
    //             let gid = range.GlobalID0
    //             let mutexIdx = gid % 4
    //             let mutable iterCount = 0
    //             let mutable flag = true
    //             let mutable s = 0
    //             while flag do
    //                 let old = mutexArray.[mutexIdx] <!> 1
    //                 printfn "%i - %i, %i" gid old iterCount
    //                 if old = 0 then
    //                     buffer.[gid] <- iterCount
    //                     mutexArray.[mutexIdx] <! 0
    //                     flag <- false
    //                 else
    //                     iterCount <- iterCount + 1
    //             barrier ()
    //     @>

    // opencl {
    //     let mutexArray = Array.zeroCreate<int> 4
    //     let buffer = Array.zeroCreate<int> 16
    //     do! runCommand kernel <| fun prepare ->
    //         prepare
    //         <| _1D(16, 8)
    //         <| mutexArray
    //         <| buffer

    //     let! _ = toHost mutexArray
    //     let! _ = toHost buffer

    //     return mutexArray, buffer
    // }
    // |> OpenCLEvaluationContext(deviceType = DeviceType.Gpu).RunSync
    // |> printfn "%A"

    // printfn "%A" <| Utils.openclTranslate k

    0

// TODO ...
(*
    - нельзя unit возвращать
    - нужно перенести осносные штуки, которые нужны для рабты в 1 нэймспейс
      (barrier и атомарные операции и DeviceType, например)
    - переименовать название рэнжей
    - сделать версию toHost, которая ничего не возвращает
*)
