module Atomic

open Expecto
open Brahma.FSharp.OpenCL
open FSharp.Quotations.Evaluator
open FSharp.Quotations
open Brahma.FSharp.Tests.Utils
open Brahma.FSharp.Tests.CustomDatatypes
open Expecto.Logging
open Expecto.Logging.Message
open ExpectoFsCheck
open FsCheck
open Brahma.FSharp.Tests

// TODO add tests in inc dec on supported types (generate spinlock)

let logger = Log.create "AtomicTests"

type NormalizedFloatArray =
    static member FloatType() =
        Arb.Default.NormalFloat()
        |> Arb.toGen
        |> Gen.map float
        |> Gen.filter
            (fun v ->
                v < 100. &&
                v > -100.
            )
        |> Gen.arrayOf
        |> Arb.fromGen

    static member Float32Type() =
        Arb.Default.Float32()
        |> Arb.toGen
        |> Gen.filter
            (fun v ->
                v < 100.f &&
                v > -100.f
            )
        |> Gen.arrayOf
        |> Arb.fromGen

module Settings =
    let wgSize = 256
    let doubledWgSize = wgSize * 2

    let fscheckConfig =
        { Config.QuickThrowOnFailure with
            QuietOnSuccess = true
            MaxTest = 20
            Arbitrary = [typeof<NormalizedFloatArray>]
        }

[<AutoOpen>]
module Helpers =
    let checkDefault<'a when 'a : equality and 'a : struct> expected kernel =
        let actual =
            opencl {
                use! result = ClArray.toDevice <| Array.zeroCreate<'a> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| Range1D(Settings.doubledWgSize, Settings.wgSize)
                    <| result

                return! ClArray.toHost result
            }
            |> ClTask.runSync context
            |> fun result -> result.[0]

        "Results should be equal"
        |> Expect.equal actual expected

/// Stress test for unary atomic operations.
/// Use global atomics
let stressTest<'a when 'a : equality and 'a : struct> (f: Expr<'a -> 'a>) size rawF (isEqual: 'a -> 'a -> bool) =
    let kernel =
        <@
            fun (range: Range1D) (result: 'a clarray) ->
                let gid = range.GlobalID0
                if gid < size then
                    atomic %f result.[0] |> ignore

                barrierLocal ()
        @>

    let expected =
        [0 .. size - 1]
        |> List.fold (fun state _ -> rawF state) Unchecked.defaultof<'a>

    let actual =
        opencl {
            use! result = ClArray.toDevice <| Array.zeroCreate<'a> 1
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| Range1D.CreateValid(size, Settings.wgSize)
                <| result

            return! ClArray.toHost result
        }
        |> ClTask.runSync context
        |> fun result -> result.[0]

    logger.debug (
        eventX "\nActual: {act}.\nExpected: {exp}"
        >> setField "act" actual
        >> setField "exp" expected
    )

    "Results should be equal"
    |> Expect.isTrue (isEqual actual expected)

/// Test for add and sub like atomic operations.
/// Use local and global atomics,
/// use reading from global mem in local atomic
let foldTest<'a when 'a : equality and 'a : struct> f (isEqual: 'a -> 'a -> bool) =
    let (.=.) left right = isEqual left right |@ sprintf "%A = %A" left right

    Check.One(Settings.fscheckConfig, fun (array: 'a[]) ->
        let arrayLength = array.Length
        let kernel zero =
            <@
                fun (range: Range1D) (array: 'a clarray) (result: 'a clarray) ->
                    let lid = range.LocalID0
                    let gid = range.GlobalID0

                    let localResult = localArray<'a> 1
                    if lid = 0 then
                        localResult.[0] <- zero

                    if gid < arrayLength then
                        atomic %f localResult.[0] array.[gid] |> ignore

                    barrierLocal ()

                    if lid = 0 then
                        atomic %f result.[0] localResult.[0] |> ignore
            @>

        let expected () =
            array
            |> Array.fold (fun state x -> f.Evaluate() state x) Unchecked.defaultof<'a>

        let actual () =
            opencl {
                use! result = ClArray.toDevice <| Array.zeroCreate<'a> 1
                use! array = ClArray.toDevice array
                do! runCommand (kernel Unchecked.defaultof<'a>) <| fun kernelPrepare ->
                    kernelPrepare
                    <| Range1D.CreateValid(array.Length, Settings.wgSize)
                    <| array
                    <| result

                return! ClArray.toHost result
            }
            |> ClTask.runSync context
            |> fun result -> result.[0]

        array.Length <> 0
        ==> lazy (actual () .=. expected ())
    )

let xchgTest<'a when 'a : equality and 'a : struct> cmp value =
    let localSize = Settings.wgSize
    let kernel =
        <@
            fun (range: Range1D) (array: 'a clarray) ->
                let gid = range.GlobalID0

                let localBuffer = localArray<'a> localSize
                atomic xchg localBuffer.[gid] array.[gid] |> ignore
                atomic cmpxchg array.[gid] cmp localBuffer.[localSize - gid] |> ignore
        @>

    let expected = Array.create<'a> localSize value

    let actual =
        opencl {
            use! buffer = ClArray.toDevice [| for i = 0 to localSize - 1 do if i < localSize / 2 then cmp else value |]
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| Range1D(localSize, localSize)
                <| buffer

            return! ClArray.toHost buffer
        }
        |> ClTask.runSync context

    "Results should be equal"
    |> Expect.sequenceEqual actual expected

let stressTestCases = testList "Stress tests" [
    let range = [1 .. 10 .. 100]

    // int
    yield! range |> List.map (fun size ->
        testCase (sprintf "Smoke stress test (size %i) on atomic 'inc' on int" size) <| fun () ->
            stressTest<int> <@ inc @> size (fun x -> x + 1) (=)
    )
    yield! range |> List.map (fun size ->
        testCase (sprintf "Smoke stress test (size %i) on atomic 'dec' on int" size) <| fun () ->
            stressTest<int> <@ dec @> size (fun x -> x - 1) (=)
    )

    // float
    yield! range |> List.map (fun size ->
        testCase (sprintf "Smoke stress test (size %i) on atomic 'inc' on float32" size) <| fun () ->
            stressTest<float32> <@ fun x -> x + 1.f @> size (fun x -> x + 1.f) (fun x y -> float (abs (x - y)) < Accuracy.low.relative)
    )

    // double
    yield! range |> List.map (fun size ->
        testCase (sprintf "Smoke stress test (size %i) on atomic 'inc' on float" size) <| fun () ->
            stressTest<float> <@ fun x -> x + 1. @> size (fun x -> x + 1.) (fun x y -> abs (x - y) < Accuracy.low.relative)
    )

    // bool
    yield! range |> List.map (fun size ->
        testCase (sprintf "Smoke stress test (size %i) on atomic 'not' on bool" size) <| fun () ->
            stressTest<bool> <@ not @> size not (=)
    )

    // WrappedInt (не работает транляция или типа того)
    let wrappedIntInc = <@ fun x -> x + WrappedInt(1) @>
    yield! range |> List.map (fun size ->
        ptestCase (sprintf "Smoke stress test (size %i) on custom atomic 'inc' on WrappedInt" size) <| fun () ->
            stressTest<WrappedInt> wrappedIntInc size (fun x -> x + WrappedInt(1)) (=)
    )

    // custom int op
    let incx2 = <@ fun x -> x + 2 @>
    yield! range |> List.map (fun size ->
        testCase (sprintf "Smoke stress test (size %i) on atomic unary func on int" size) <| fun () ->
            stressTest<int> incx2 size (fun x -> x + 2) (=)
    )
]

let foldTestCases = testList "Fold tests" [
    // int, smoke tests
    testCase "Smoke fold test atomic 'add' on int" <| fun () -> foldTest<int> <@ (+) @> (=)

    // float
    testCase "Fold test atomic 'add' on float32" <| fun () -> foldTest<float32> <@ (+) @> (fun x y -> float (abs (x - y)) < Accuracy.low.relative)

    // double
    testCase "Fold test atomic 'add' on float" <| fun () -> foldTest<float> <@ (+) @> (fun x y -> abs (x - y) < Accuracy.low.relative)

    // bool
    // error: bool can't be used as kernel argument in OpenCL
    ptestCase "Fold test atomic '&&' on bool" <| fun () -> foldTest<bool> <@ (&&) @> (=)

    testCase "Reduce test atomic 'min' on int" <| fun () -> foldTest<int> <@ min @> (=)
    ptestCase "Reduce test atomic 'min' on int64" <| fun () -> foldTest<int64> <@ min @> (=)
    testCase "Reduce test atomic 'min' on int16" <| fun () -> foldTest<int16> <@ min @> (=)

    testCase "Reduce test atomic 'max' on int" <| fun () -> foldTest<int> <@ max @> (=)
    ptestCase "Reduce test atomic 'max' on int64" <| fun () -> foldTest<int64> <@ max @> (=)
    testCase "Reduce test atomic 'max' on int16" <| fun () -> foldTest<int16> <@ max @> (=)

    testCase "Reduce test atomic '&&&' on int" <| fun () -> foldTest<int> <@ (&&&) @> (=)
    ptestCase "Reduce test atomic '&&&' on int64" <| fun () -> foldTest<int64> <@ (&&&) @> (=)

    testCase "Reduce test atomic '|||' on int" <| fun () -> foldTest<int> <@ (|||) @> (=)
    ptestCase "Reduce test atomic '|||' on int64" <| fun () -> foldTest<int64> <@ (|||) @> (=)

    testCase "Reduce test atomic '^^^' on int" <| fun () -> foldTest<int> <@ (^^^) @> (=)
    ptestCase "Reduce test atomic '^^^' on int64" <| fun () -> foldTest<int64> <@ (^^^) @> (=)

    // WrappedInt (не работает транляция или типа того)
    ptestCase "Fold test atomic 'add' on WrappedInt" <| fun () -> foldTest<WrappedInt> <@ (+) @> (=)
]

let xchgTestCases = testList "Xchg tests" [
    testCase "Xchg test on int" <| fun () -> xchgTest<int> 0 256
    testCase "Xchg test on float" <| fun () -> xchgTest<float> 0. 256.
    testCase "Xchg test on bool" <| fun () -> xchgTest<bool> false true
    ptestCase "Xchg test on WrappedInt" <| fun () -> xchgTest<WrappedInt> (WrappedInt 0) (WrappedInt 256)
]

// TODO barrier broken
let perfomanceTest = ptestCase "Perfomance test on 'inc'" <| fun () ->
    // use native atomic_inc for int
    let kernelUsingNativeInc =
        <@
            fun (range: Range1D) (result: int clarray) ->
                let localAcc = localArray<int> 1
                if range.LocalID0 = 0 then
                    localAcc.[0] <- 0

                atomic inc localAcc.[0] |> ignore
                barrierLocal ()

                if range.LocalID0 = 0 then
                    result.[0] <- localAcc.[0]
        @>

    // generate spinlock
    let kernelUsingCustomInc =
        let inc = <@ fun x -> x + 1 @>
        <@
            fun (range: Range1D) (result: int clarray) ->
                let localAcc = localArray<int> 1
                if range.LocalID0 = 0 then
                    localAcc.[0] <- 0

                atomic %inc localAcc.[0] |> ignore
                barrierLocal ()

                if range.LocalID0 = 0 then
                    result.[0] <- localAcc.[0]
        @>

    let prepare kernel () =
        opencl {
            use! result = ClArray.toDevice <| Array.zeroCreate<int> 1
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| Range1D(Settings.wgSize, Settings.wgSize)
                <| result

            return! ClArray.toHost result
        }
        |> ClTask.runSync context

    "Kernel wich uses native 'inc' should be faster than with custom one"
    |> Expect.isFasterThan (prepare kernelUsingNativeInc) (prepare kernelUsingCustomInc)

// let commonTests = testList "Behavior/semantic tests" [
//     testCase "Srtp test on 'add'" <| fun () ->
//         let inline kernel () =
//             <@
//                 fun (range: Range1D) (result: 'a[]) (value: 'a) ->
//                     atomic (+) result.[0] value |> ignore
//             @>

//         let srtpOnIntActual = finalize <| fun () ->
//             opencl {
//                 let result = Array.zeroCreate<int> 1
//                 do! runCommand (kernel ()) <| fun kernelPrepare ->
//                     kernelPrepare
//                     <| Range1D(Settings.doubledWgSize, Settings.wgSize)
//                     <| result
//                     <| 1

//                 return! toHost result
//             }
//             |> context.RunSync
//             |> fun result -> result.[0]

//         let srtpOnFloatActual = finalize <| fun () ->
//             opencl {
//                 let result = Array.zeroCreate<float> 1
//                 do! runCommand (kernel ()) <| fun kernelPrepare ->
//                     kernelPrepare
//                     <| Range1D(Settings.doubledWgSize, Settings.wgSize)
//                     <| result
//                     <| 1.

//                 return! toHost result
//             }
//             |> context.RunSync
//             |> fun result -> result.[0]

//         "Results should be equal up to types"
//         |> Expect.isTrue (float srtpOnIntActual = srtpOnFloatActual)

//     testCase "Check sequential fully equal atomic operations (native)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (result: int[]) ->
//                     atomic (+) result.[0] 1 |> ignore
//                     atomic (+) result.[0] 1 |> ignore
//             @>

//         let expected = Settings.doubledWgSize * 2
//         kernel |> checkDefault<int> expected

//     testCase "Check sequential fully equal atomic operations (spinlock)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (result: int[]) ->
//                     atomic (fun x -> x + 1) result.[0] |> ignore
//                     atomic (fun x -> x + 1) result.[0] |> ignore
//             @>

//         let expected = Settings.doubledWgSize * 2
//         kernel |> checkDefault<int> expected

//     testCase "Check sequential equal atomic operations but with different types (spinlock)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (resultInt: int[]) (resultFloat32: float[]) ->
//                     atomic (fun x -> x + 1) resultInt.[0] |> ignore
//                     atomic (fun x -> x + 1.) resultFloat32.[0] |> ignore
//             @>

//         let expected = (Settings.doubledWgSize, float Settings.doubledWgSize)

//         let actual = finalize <| fun () ->
//             opencl {
//                 let resultInt = Array.zeroCreate<int> 1
//                 let resultFloat = Array.zeroCreate<float> 1
//                 do! runCommand kernel <| fun kernelPrepare ->
//                     kernelPrepare
//                     <| Range1D(Settings.doubledWgSize, Settings.wgSize)
//                     <| resultInt
//                     <| resultFloat

//                 do! transferToHost resultInt
//                 do! transferToHost resultFloat
//                 return (resultInt, resultFloat)
//             }
//             |> context.RunSync
//             |> fun (resultInt, resultFloat32) -> (resultInt.[0], resultFloat32.[0])

//         "Results should be equal"
//         |> Expect.equal actual expected

//     // TODO barrier broken
//     ptestCase "Check sequential equal atomic operations but different address qualifiers (spinlock)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (result: int[]) ->
//                     let localResult = localArray<int> 1
//                     if range.LocalID0 = 0 then
//                         localResult.[0] <- 0

//                     atomic (fun x -> x + 1) result.[0] |> ignore
//                     atomic (fun x -> x + 1) localResult.[0] |> ignore
//                     barrier ()

//                     if range.LocalID0 = 0 then
//                         result.[0] <- result.[0] + localResult.[0]
//             @>

//         let expected = Settings.wgSize * 2

//         let actual = finalize <| fun () ->
//             opencl {
//                 let result = Array.zeroCreate<int> 1
//                 do! runCommand kernel <| fun kernelPrepare ->
//                     kernelPrepare
//                     <| Range1D(Settings.wgSize, Settings.wgSize)
//                     <| result

//                 return! toHost result
//             }
//             |> context.RunSync
//             |> fun result -> result.[0]

//         "Results should be equal"
//         |> Expect.equal actual expected

//     // TODO barrier broken
//     ptestCase "Check sequential equal atomic operations on local array (spinlock)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (result: int[]) ->
//                     let localSingleton = localArray<int> 1
//                     if range.LocalID0 = 0 then
//                         localSingleton.[0] <- 0

//                     atomic (fun x -> x + 1) localSingleton.[0] |> ignore
//                     atomic (fun x -> x + 1) localSingleton.[0] |> ignore
//                     barrier ()

//                     if range.LocalID0 = 0 then
//                         result.[0] <- localSingleton.[0]
//             @>

//         let expected = Settings.wgSize * 2

//         let actual = finalize <| fun () ->
//             opencl {
//                 let result = Array.zeroCreate<int> 1
//                 do! runCommand kernel <| fun kernelPrepare ->
//                     kernelPrepare
//                     <| Range1D(Settings.wgSize, Settings.wgSize)
//                     <| result

//                 return! toHost result
//             }
//             |> context.RunSync
//             |> fun result -> result.[0]

//         "Results should be equal"
//         |> Expect.equal actual expected

//     testCase "Check atomic inside lambda (explicit param, native)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (result: int[]) ->
//                     let f x = atomic (+) result.[0] x
//                     f 1 |> ignore
//             @>

//         let expected = Settings.doubledWgSize
//         kernel |> checkDefault<int> expected

//     testCase "Check atomic inside lambda (explicit param, spinlock)" <| fun () ->
//         let kernel =
//             <@
//                 fun (range: Range1D) (result: int[]) ->
//                     let f y = atomic (fun x y -> x + y + 1) result.[0] y
//                     f 1 |> ignore
//             @>

//         let expected = Settings.doubledWgSize * 2
//         kernel |> checkDefault<int> expected
// ]

let tests =
    testList "Tests on atomic functions" [
        stressTestCases
        foldTestCases
        perfomanceTest
        // commonTests
    ]
    |> testSequenced

