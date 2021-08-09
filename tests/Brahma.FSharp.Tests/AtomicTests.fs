module Atomic

open Expecto
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.WorkflowBuilder
open FSharp.Quotations.Evaluator
open Brahma.FSharp.Tests.Utils
open Brahma.FSharp.Tests.CustomDatatypes
open Expecto.Logging
open Expecto.Logging.Message

let logger = Log.create "AtomicTests"

module Settings =
    let wgSize = 256
    let doubledWgSize = wgSize * 2
    let getValidGS = getValidGlobalSize wgSize

/// Stress test for unary atomic operations.
/// Use global atomics
let stressTest<'a when 'a : equality> f size =
    let kernel =
        <@
            fun (range: _1D) (result: 'a[]) ->
                atomic %f result.[0] |> ignore
        @>

    let expected =
        [0 .. size - 1]
        |> List.fold (fun state _ -> f.Evaluate() state) Unchecked.defaultof<'a>

    let actual = finalize <| fun () ->
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

                let localResult = localArray<'a> 1
                atomic %f localResult.[0] array.[lid] |> ignore
                barrier ()

                if lid = 0 then
                    atomic %f result.[0] localResult.[0] |> ignore
        @>

    let expected =
        array
        |> Array.fold (fun state x -> f.Evaluate() state x) Unchecked.defaultof<'a>

    let actual = finalize <| fun () ->
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

    "Results should be equal"
    |> Expect.equal actual expected

/// Test for reduce like atomic operations.
/// Use global atomics and non-atomic version of operation.
let reduceTest<'a when 'a : equality> f (array: 'a[]) =
    let localSize = Settings.wgSize
    let kernel =
        <@
            fun (range: _1D) (array: 'a[]) (result: 'a[]) ->

                let lid = range.LocalID0
                let gid = range.GlobalID0

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

    let actual = finalize <| fun () ->
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

    "Results should be equal"
    |> Expect.equal actual expected

// TODO Tests for xchg и cmpxchg

let stressTestCases = testList "Stress tests" [
    let range = [0 .. 10 .. 100]

    // int
    yield! range |> List.map (fun size ->
    testCase "Smoke stress test atomic inc on int" <| fun () -> stressTest<int> <@ inc @> size)
    yield! range |> List.map (fun size ->
    testCase "Smoke stress test atomic dec on int" <| fun () -> stressTest<int> <@ dec @> size)

    // float
    yield! range |> List.map (fun size ->
    testCase "Stress test atomic inc on float32" <| fun () -> stressTest<float32> <@ inc @> size)

    // double
    yield! range |> List.map (fun size ->
    testCase "Stress test atomic inc on float" <| fun () -> stressTest<float> <@ inc @> size)

    // bool
    yield! range |> List.map (fun size ->
    testCase "Stress test atomic 'not' on bool" <| fun () -> stressTest<bool> <@ not @> size)

    // WrappedInt (???)
    let wrappedIntInc = <@ fun x -> x + WrappedInt(1) @>
    yield! range |> List.map (fun size ->
    testCase "Stress test custom atomic inc on WrappedInt" <| fun () -> stressTest<WrappedInt> wrappedIntInc size)

    // custom int op
    let incx2 = <@ fun x -> x + 2 @>
    yield! range |> List.map (fun size ->
    testCase "Stress test custom atomic unary func on int" <| fun () -> stressTest<int> incx2 size)
]

let foldTestCases = testList "Fold tests" [
    // int, smoke tests
    foldTest<int> <@ (+) @> |> testProperty "Smoke fold test atomic add on int"
    foldTest<int> <@ (-) @> |> testProperty "Smoke fold test atomic sub on int"

    // float
    foldTest<float32> <@ (+) @> |> testProperty "Fold test atomic add on float32"

    // double
    foldTest<float> <@ (+) @> |> testProperty "Fold test atomic add on float"

    // bool
    foldTest<bool> <@ (&&) @> |> testProperty "Fold test atomic && on bool"

    // WrappedInt
    foldTest<WrappedInt> <@ (+) @> |> testProperty "Fold test atomic add on WrappedInt"

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

    reduceTest<int> <@ (&&&) @> |> testProperty "Reduce test atomic &&& on int"
    reduceTest<int64> <@ (&&&) @> |> testProperty "Reduce test atomic &&& on int64"

    reduceTest<int> <@ (|||) @> |> testProperty "Reduce test atomic ||| on int"
    reduceTest<int64> <@ (|||) @> |> testProperty "Reduce test atomic ||| on int64"

    reduceTest<int> <@ (^^^) @> |> testProperty "Reduce test atomic ^^^ on int"
    reduceTest<int64> <@ (^^^) @> |> testProperty "Reduce test atomic ^^^ on int64"
]

let perfomanceTest = testCase "Perfomance test on inc" <| fun () ->
    // use native atomic_inc for int
    let kernelUsingNativeInc () = finalize <| fun () ->
        opencl {
            let kernel =
                <@
                    fun (range: _1D) (result: int[]) ->
                        let localAcc = localArray<int> 1
                        atomic inc localAcc.[0] |> ignore
                        barrier ()

                        if range.LocalID0 = 0 then
                            result.[0] <- localAcc.[0]
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
    let kernelUsingCustomInc () = finalize <| fun () ->
        opencl {
            let inc = <@ fun x -> x + 1 @>
            let kernel =
                <@
                    fun (range: _1D) (result: int[]) ->
                        let localAcc = localArray<int> 1
                        atomic %inc localAcc.[0] |> ignore
                        barrier ()

                        if range.LocalID0 = 0 then
                            result.[0] <- localAcc.[0]
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

// TODO deadlock test

let commonTests = testList "Behavior/semantic tests" [
    testCase "Check operation definition inside quotation" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (result: int[]) ->
                    let incx2 x = x + 2
                    atomic incx2 result.[0] |> ignore
            @>

        let size = Settings.wgSize * 2

        let expected =
            [0 .. size - 1]
            |> List.fold (fun state _ -> state + 2) 0

        let actual = finalize <| fun () ->
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

        "Results should be equal"
        |> Expect.equal actual expected

    testCase "Srtp test on inc" <| fun () ->
        let inline kernel () =
            <@
                fun (range: _1D) (result: 'a[]) ->
                    atomic inc result.[0] |> ignore
            @>

        let srtpOnIntActual = finalize <| fun () ->
            opencl {
                let result = Array.zeroCreate<int> 1
                do! runCommand (kernel ()) <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.doubledWgSize, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        let srtpOnFloatActual = finalize <| fun () ->
            opencl {
                let result = Array.zeroCreate<float> 1
                do! runCommand (kernel ()) <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.doubledWgSize, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        "Results should be equal up to types"
        |> Expect.isTrue (float srtpOnIntActual = srtpOnFloatActual)

    testCase "Check sequential fully equal atomic operations" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (result: int[]) ->
                    atomic inc result.[0] |> ignore
                    atomic inc result.[0] |> ignore
            @>

        let expected = Settings.doubledWgSize * 2

        let actual = finalize <| fun () ->
            opencl {
                let result = Array.zeroCreate<int> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.doubledWgSize, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        "Results should be equal"
        |> Expect.equal actual expected

    testCase "Check sequential equal atomic operations but with different types" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (resultInt: int[]) (resultFloat32: float32[]) ->
                    atomic inc resultInt.[0] |> ignore
                    atomic inc resultFloat32.[0] |> ignore
            @>

        let expected = (Settings.doubledWgSize, float32 Settings.doubledWgSize)

        let actual = finalize <| fun () ->
            opencl {
                let resultInt = Array.zeroCreate<int> 1
                let resultFloat32 = Array.zeroCreate<float32> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.doubledWgSize, Settings.wgSize)
                    <| resultInt
                    <| resultFloat32

                do! transferToHost resultInt
                do! transferToHost resultFloat32
                return (resultInt, resultFloat32)
            }
            |> context.RunSync
            |> fun (resultInt, resultFloat32) -> (resultInt.[0], resultFloat32.[0])

        "Results should be equal"
        |> Expect.equal actual expected

    testCase "Check sequential equal atomic operations but different address qualifiers" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (result: int[]) ->
                    let localResult = localArray<int> 1
                    atomic inc result.[0] |> ignore
                    atomic inc localResult.[0] |> ignore
                    barrier ()
                    if range.GlobalID0 = 0 then
                        result.[0] <- result.[0] + localResult.[0]
            @>

        let expected = Settings.wgSize * 2

        let actual = finalize <| fun () ->
            opencl {
                let result = Array.zeroCreate<int> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.wgSize, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        "Results should be equal"
        |> Expect.equal actual expected

    // NOTE не умеем toHost если не массив
    // NOTE не массивы и не ref параметры в приватной памяти -- не консистентное поведение с Local (local тоже тогда ref долен возвращать??)
    // testCase "Check atomic operation on global non-array object" <| fun () ->
    //     let kernel =
    //         <@
    //             fun (range: _1D) (result: int) ->
    //                 atomic inc result |> ignore
    //         @>

    //     let expected = Settings.doubledWgSize

    //     let actual = finalize <| fun () ->
    //         opencl {
    //             let result = 0
    //             do! runCommand kernel <| fun kernelPrepare ->
    //                 kernelPrepare
    //                 <| _1D(Settings.doubledWgSize, Settings.wgSize)
    //                 <| result

    //             return! toHost result
    //         }
    //         |> context.RunSync
    //         |> fun result -> result.[0]

    //     "Results should be equal"
    //     |> Expect.equal actual expected

    testCase "Check atomic operation as guard in WHILE loop" <| fun () ->
        let maxAcc = 10
        let kernel =
            <@
                fun (range: _1D) (array: int[]) ->
                    while atomic inc array.[0] <> maxAcc do
                        1 |> ignore // cause () unsupported
            @>

        let expected = maxAcc

        let actual = finalize <| fun () ->
            opencl {
                let result = Array.zeroCreate<int> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.doubledWgSize, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        "Results should be equal"
        |> Expect.equal actual expected

    testCase "Check sequential equal atomic operations on local array" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (result: int[]) ->
                    let localSingleton = localArray<int> 1
                    atomic inc localSingleton.[0] |> ignore
                    atomic inc localSingleton.[0] |> ignore
                    barrier ()
                    if range.GlobalID0 = 0 then
                        result.[0] <- localSingleton.[0]
            @>

        let expected = Settings.wgSize * 2

        let actual = finalize <| fun () ->
            opencl {
                let result = Array.zeroCreate<int> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| _1D(Settings.wgSize, Settings.wgSize)
                    <| result

                return! toHost result
            }
            |> context.RunSync
            |> fun result -> result.[0]

        "Results should be equal"
        |> Expect.equal actual expected

    // TODO
    testCase "Check atomic inside lambda, v1" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (result: int[]) ->
                    let f x = atomic (+) result.[0] x
                    f 1 |> ignore
            @>
        ()

    testCase "Check atomic inside lambda, v2" <| fun () ->
        let kernel =
            <@
                fun (range: _1D) (result: int[]) ->
                    let f = atomic (+) result.[0]
                    f 1 |> ignore
            @>
        ()
]

let tests =
    testList "Tests on atomic functions" [
        stressTestCases
        foldTestCases
        reduceTestCases
        perfomanceTest
        commonTests
    ]
    |> testSequenced
