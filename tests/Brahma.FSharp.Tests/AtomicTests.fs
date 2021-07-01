module Atomic

open Expecto
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
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

/// Stress test for unary atomic operations.
/// Use global atomics
let stressTest<'a when 'a : equality> f size =
    let kernel =
        <@
            fun (range: _1D) (result: 'a[]) ->
                atomic %f result.[0] |> ignore
                barrier ()
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

/// Test for add ans sub like atomic operations.
/// Use local and global atomics,
/// use reading from global mem in local atomic
let foldTest<'a when 'a : equality and 'a : struct> f (array: 'a[]) =
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


/// Test for reduce like atomic operations.
/// Use global atomics and non-atomic version of operation.
let reduceTest<'a when 'a : equality> f (array: 'a[]) =
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


// TODO Tests for xchg и cmpxchg


// TODO remake cause ptestCase
let stressTestCases = testList "Stress tests" [
    let range = [0 .. 10 .. 100]
    let makeTestCaseWithName name =
        fun testBody -> (fun () -> testBody)
        >> testCase name

    // int
    yield! range |> List.map ((stressTest<int> <@ inc @>) >> makeTestCaseWithName "Smoke stress test atomic inc on int")
    yield! range |> List.map ((stressTest<int> <@ dec @>) >> makeTestCaseWithName "Smoke stress test atomic dec on int")

    // float
    yield! range |> List.map ((stressTest<float32> <@ inc @>) >> makeTestCaseWithName "Stress test atomic inc on float32")

    // double
    yield! range |> List.map ((stressTest<float> <@ inc @>) >> makeTestCaseWithName "Stress test atomic inc on float")

    // bool
    yield! range |> List.map ((stressTest<bool> <@ not @>) >> makeTestCaseWithName "Stress test atomic 'not' on bool")

    // WrappedInt (???)
    let wrappedIntInc = <@ fun x -> x + { InnerValue = 1 } @>
    yield! range |> List.map ((stressTest<WrappedInt> wrappedIntInc) >> makeTestCaseWithName "Stress test custom atomic inc on WrappedInt")

    // custom int op
    let incx2 = <@ fun x -> x + 2 @>
    yield! range |> List.map ((stressTest<int> incx2) >> makeTestCaseWithName "Stress test custom atomic unary func on int")
]

let foldTestCases = testList "Fold tests" [
    // int, smoke tests
    foldTest<int> <@ add @> |> testProperty "Smoke fold test atomic add on int"
    foldTest<int> <@ sub @> |> testProperty "Smoke fold test atomic sub on int"

    // float
    foldTest<float32> <@ add @> |> testProperty "Fold test atomic add on float32"

    // double
    foldTest<float> <@ add @> |> testProperty "Fold test atomic add on float"

    // bool
    foldTest<bool> <@ (&&) @> |> testProperty "Fold test atomic && on bool"

    // WrappedInt
    foldTest<WrappedInt> <@ add @> |> testProperty "Fold test atomic add on WrappedInt"

    // custom int op
    let y2x = <@ fun x y -> y + x + x @>
    foldTest<int> y2x |> testProperty "Fold test custom atomic operation on int"
]

let reduceTestCases = testList "Reduce tests" [
    reduceTest<int> <@ min @> |> testProperty "Reduce test atomic min on int"
    reduceTest<float32> <@ min @> |> testProperty "Reduce test atomic min on float32"
    reduceTest<float> <@ min @> |> testProperty "Reduce test atomic min on float"

    reduceTest<int> <@ max @> |> testProperty "Reduce test atomic max on int"
    reduceTest<float32> <@ max @> |> testProperty "Reduce test atomic max on float32"
    reduceTest<float> <@ max @> |> testProperty "Reduce test atomic max on float"

    reduceTest<int> <@ and' @> |> testProperty "Reduce test atomic &&& on int"
    reduceTest<int64> <@ and' @> |> testProperty "Reduce test atomic &&& on int64"

    reduceTest<int> <@ or' @> |> testProperty "Reduce test atomic ||| on int"
    reduceTest<int64> <@ or' @> |> testProperty "Reduce test atomic ||| on int64"

    reduceTest<int> <@ xor @> |> testProperty "Reduce test atomic ^^^ on int"
    reduceTest<int64> <@ xor @> |> testProperty "Reduce test atomic ^^^ on int64"
]

let atomicInsideQuotTest = testCase "Operation definition inside quotation" <| fun () ->
    let kernel =
        <@
            fun (range: _1D) (result: int[]) ->
                let incx2 x = x + 2
                atomic incx2 result.[0] |> ignore
                barrier ()
        @>

    let size = Settings.wgSize * 2

    let expected =
        [0 .. size - 1]
        |> List.fold (fun state _ -> state + 2) 0

    let actual =
        try
            opencl {
                let result = Array.zeroCreate<int> 1
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

let perfomanceTest = testCase "" <| fun () ->
    // use native atomic_inc for int
    let kernelUsingNativeInc () =
        opencl {
            let kernel =
                <@
                    fun (range: _1D) (result: int[]) ->
                        let localAcc = local<int> ()
                        atomic inc localAcc |> ignore
                        barrier ()

                        if range.LocalID0 = 0 then
                            result.[0] <- localAcc
                @>

            let result = Array.zeroCreate<int> 1
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| _1D(Settings.wgSize, Settings.wgSize)
                <| result

            return! toHost result
        }
        |> context.RunSync

    // generate spin lock
    let kernelUsingCustomInc () =
        opencl {
            let inc = <@ fun x -> x + 1 @>
            let kernel =
                <@
                    fun (range: _1D) (result: int[]) ->
                        let localAcc = local<int> ()
                        atomic %inc localAcc |> ignore
                        barrier ()

                        if range.LocalID0 = 0 then
                            result.[0] <- localAcc
                @>

            let result = Array.zeroCreate<int> 1
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| _1D(Settings.wgSize, Settings.wgSize)
                <| result

            return! toHost result
        }
        |> context.RunSync

    "Kernel wich uses native inc shold be faster than with custom one"
    |> Expect.isFasterThan kernelUsingNativeInc kernelUsingCustomInc

let tests =
    testList "Tests on atomic functions" [
        stressTestCases
        foldTestCases
        reduceTestCases
        atomicInsideQuotTest
        perfomanceTest
    ] |> testSequenced
