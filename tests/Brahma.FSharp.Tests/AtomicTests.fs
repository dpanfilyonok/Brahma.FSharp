module Atomic

open Expecto
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.WorkflowBuilder
open FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.Tests.Utils
open Brahma.FSharp.Tests.CustomDatatypes
open Expecto.Logging
open Expecto.Logging.Message
open FsCheck

let logger = Log.create "AtomicTests"

module Settings =
    let wgSize = 256
    let getValidGS = getValidGlobalSize wgSize

// одновременный запрос на доступ из очень многих потоков
// не нужно делать дженериковым
// не нужно проверять разные по сути опеарции
// нужно потестить, как производительность зависит от слоности операции
// fold
// или проверить dec и inc
// inc на интах должны быть быстее чем на даблах
// inc and dec
// gloab atomic
let stressTest<'a when 'a : equality> f size =
    let kernel =
        <@
            fun (range: _1D) (result: 'a[]) ->
                atomic %f result.[0] |> ignore
        @>

    let expected =
        [0 .. size - 1]
        |> List.fold (fun state _ -> f.Evaluate() state) Unchecked.defaultof<'a>

    let actual =
        try
            opencl {
                let result = Array.zeroCreate<'a> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.getValidGS size, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        finally
            context.Provider.CloseAllBuffers()

    "Results should be equal"
    |> Expect.equal actual expected

// sum and sub
// global and local atomic
let foldTest<'a when 'a : equality and 'a : struct> f (array: 'a[]) =
    test (sprintf "Stress test on type %O" typeof<'a>) {
        let kernel =
            <@
                fun (range: _1D) (array: 'a[]) (result: 'a[]) ->
                    let lid = range.LocalID0

                    let localResult = local<'a> ()
                    // NOTE array.[lid] should be called once
                    atomic %f localResult array.[lid] |> ignore
                    barrier ()

                    if lid = 0 then
                        atomic %f result.[0] localResult  |> ignore
            @>

        let expected =
            array
            |> Array.fold (fun state x -> f.Evaluate() state x) Unchecked.defaultof<'a>

        let actual =
            try
                opencl {
                    let result = Array.zeroCreate<'a> 1
                    do! runCommand kernel <| fun kernelPrepare ->
                        kernelPrepare
                        <| _1D(Settings.getValidGS array.Length, Settings.wgSize)
                        <| array
                        <| result

                    return! toHost result
                }
                |> context.RunSync
                |> fun result -> result.[0]

            finally
                context.Provider.CloseAllBuffers()

        "Results should be equal"
        |> Expect.equal actual expected
    }


// проверить сумму на разных типах данных
// sum
// или reduce для разных операций
let reduceTest<'a when 'a : equality> f (array: 'a[]) =
    test "" {
        let localSize = Settings.wgSize
        let kernel =
            <@
                fun (ndRange: _1D) (array: 'a[]) (result: 'a[]) ->

                    let lid = ndRange.LocalID0
                    let gid = ndRange.GlobalID0

                    let localBuffer = localArray<'a> localSize
                    localBuffer.[lid] <- array.[gid]
                    barrier ()

                    let mutable amountOfValuesToSum = localSize
                    while amountOfValuesToSum > 1 do
                        if lid * 2 < amountOfValuesToSum then
                            let a = localBuffer.[lid]
                            let b = localBuffer.[lid + amountOfValuesToSum / 2]
                            localBuffer.[lid] <- (%f) a b
                        amountOfValuesToSum <- amountOfValuesToSum / 2
                        barrier ()

                    if lid = 0 then
                        atomic %f result.[0] localBuffer.[lid] |> ignore
            @>

        let expected =
            array
            |> Array.reduce (fun x y -> f.Evaluate() x y)

        let actual =
            try
                opencl {
                    let result = Array.zeroCreate<'a> 1
                    do! runCommand kernel <| fun kernelPrepare ->
                        kernelPrepare
                        <| _1D(Settings.getValidGS array.Length, Settings.wgSize)
                        <| array
                        <| result

                    return! toHost result
                }
                |> context.RunSync
                |> fun result -> result.[0]

            finally
                context.Provider.CloseAllBuffers()

        "Results should be equal"
        |> Expect.equal actual expected
    }

// xchg и cmpxchg
let lol = ()

let stressTestCases = testList "Stress tests" [
    let range = [0 .. 10 .. 100]
    let makeTestCaseWithName name =
        fun testBody -> (fun () -> testBody)
        >> testCase name

    // int
    yield! range |> List.map ((stressTest<int> <@ inc @>) >> makeTestCaseWithName "Smoke test atomic inc on int")
    yield! range |> List.map ((stressTest<int> <@ dec @>) >> makeTestCaseWithName "Smoke test atomic dec on int")

    // float
    yield! range |> List.map ((stressTest<float32> <@ inc @>) >> makeTestCaseWithName "Test atomic inc on float32")

    // double
    yield! range |> List.map ((stressTest<float> <@ inc @>) >> makeTestCaseWithName "Test atomic inc on float")

    // bool
    yield! range |> List.map ((stressTest<bool> <@ not @>) >> makeTestCaseWithName "Test atomic 'not' on bool")

    // WrappedInt (???)
    let wrappedIntInc = <@ fun x -> x + { InnerValue = 1 } @>
    yield! range |> List.map ((stressTest<WrappedInt> wrappedIntInc) >> makeTestCaseWithName "Test custom atomic inc on WrappedInt")

    // custom int op
    let incx2 = <@ fun x -> x + 2 @>
    yield! range |> List.map ((stressTest<int> incx2) >> makeTestCaseWithName "Test custom atomic unary func on int")
]

let foldTestCases = testList "" [
    // int, smoke tests
    foldTest<int> <@ add @> |> testProperty "lol"
    foldTest<int> <@ sub @> |> testProperty "lol"

    // float
    foldTest<float32> <@ add @> |> testProperty "lol"

    // double
    foldTest<float> <@ add @> |> testProperty "lol"

    // bool
    foldTest<bool> <@ (&&) @> |> testProperty "lol"

    // WrappedInt
    foldTest<WrappedInt> <@ add @> |> testProperty "lol"

    // custom int op
    let y2x = <@ fun x y -> y + x + x @>
    foldTest<int> y2x |> testProperty "lol"
]

let reduceTestCases = testList "" [
    reduceTest <@ min @> [| 1; 2 |]
    reduceTest <@ max @> [| 1; 2 |]
    reduceTest <@ and' @> [| 1; 2 |]
    reduceTest <@ or' @> [| 1; 2 |]
    reduceTest <@ xor @> [| 1; 2 |]
]

let tests =
    testList "Tests on atomic functions" [
        stressTestCases
        foldTestCases
        reduceTestCases
    ]
