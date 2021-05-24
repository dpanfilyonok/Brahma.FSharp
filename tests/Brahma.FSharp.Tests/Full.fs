module Brahma.FSharp.Tests.Full

open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open Expecto
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.WorkflowBuilder.Basic
open FSharp.Quotations

[<Struct>]
type TestStruct =
    val mutable x: int
    val mutable y: float
    new (x,y) = {x=x; y=y}

[<Tests>]
let fullTranslatorTests =
    let defaultInArrayLength = 4
    let intInArr = [|0..defaultInArrayLength-1|]
    let float32Arr = Array.init defaultInArrayLength (fun i -> float32 i)
    let _1d = new _1D(defaultInArrayLength, 1)
    let _2d = new _2D(defaultInArrayLength, 1)
    let deviceType = DeviceType.Default
    let platformName = "*"

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message

    let checkResult command =
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

        let check (outArray:array<'a>) (expected:array<'a>) =
            let r = Array.zeroCreate outArray.Length

            commandQueue.Add(kernelRunF())
                        .Add(outArray.ToHost(provider, r))
                        .Finish()
                        .Dispose()
            provider.CloseAllBuffers()

            Expect.sequenceEqual r expected "Arrays should be equals"

        kernelPrepareF,check

    let atomicsTests =
        testList "Tests on atomic functions."
            [
                testCase "Atomic exchange int" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<_>) ->
                                buf.[0] <! 1
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0; 1; 2; 3|]
                    run _1d initInArr
                    check initInArr [|1; 1; 2; 3|]

                ptestCase "Atomic exchange float" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<_>) ->
                                buf.[0] <! 1.0f
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0.0f; 1.0f; 2.0f; 3.0f|]
                    run _1d initInArr
                    check initInArr [|1.0f; 1.0f; 2.0f; 3.0f|]

                ptestCase "Atomic exchange int and float" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<_>) ->
                                let local_buf = localArray 1
                                let x = local_buf.[0] <!> int buf.[0]
                                buf.[0] <! float32 x + 1.0f
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0.0f; 1.0f; 2.0f; 3.0f|]
                    run _1d initInArr
                    check initInArr [|1.0f; 1.0f; 2.0f; 3.0f|]

                testCase "Atomic min and max int" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<_>) ->
                                let local_buf = localArray 1
                                let x = aMinR local_buf.[0] (int buf.[0])
                                aMax buf.[0] (x + 1)
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0; 1; 2; 3|]
                    run _1d initInArr
                    check initInArr [|1; 1; 2; 3|]
            ]

    let arrayItemSetTests =
        testList "Array item set tests."
            [
                testCase "Array item set" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                buf.[0] <- 1
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|1;1;2;3|]

                testCase "Array item set. Long" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<_>) ->
                                buf.[0] <- 1L
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0L; 1L; 2L; 3L|]
                    run _1d initInArr
                    check initInArr [|1L; 1L; 2L; 3L|]

                testCase "Array item set. ULong" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<uint64>) ->
                                buf.[0] <- 1UL
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0UL; 1UL; 2UL; 3UL|]
                    run _1d initInArr
                    check initInArr [|1UL; 1UL; 2UL; 3UL|]

                testCase "Array item set. Sequential operations." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                buf.[0] <- 2
                                buf.[1] <- 4
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|2;4;2;3|]

            ]

    let typeCastingTests =
        testList "Type castings tests"
            [
                testCase "Type casting. Long" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int64>) ->
                                buf.[0] <- (int64)1UL
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0L; 1L|]
                    run _1d initInArr
                    check initInArr [|1L; 1L|]

                testCase "Type casting. Ulong" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<uint64>) ->
                                buf.[0] <- 1UL
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0UL; 1UL; 2UL; 3UL|]
                    run _1d initInArr
                    check initInArr [|1UL; 1UL; 2UL; 3UL|]

                testCase "Type casting. ULong" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<uint64>) ->
                                buf.[0] <- (uint64)1L
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0UL; 1UL|]
                    run _1d initInArr
                    check initInArr [|1UL; 1UL|]

                testCase "Byte type support" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<byte>) ->
                                if range.GlobalID0 = 0
                                then
                                    buf.[0] <- buf.[0] + 1uy
                                    buf.[1] <- buf.[1] + 1uy
                                    buf.[2] <- buf.[2] + 1uy
                        @>

                    let run,check = checkResult command
                    let inByteArray = [|0uy;255uy;254uy|]
                    run _1d inByteArray
                    check inByteArray [|1uy;0uy;255uy|]

                testCase "Byte and float32" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<byte>) ->
                                if range.GlobalID0 = 0
                                then
                                    buf.[0] <- byte (float buf.[0])
                                    buf.[1] <- byte (float buf.[1])
                                    buf.[2] <- byte (float buf.[2])
                        @>

                    let run,check = checkResult command
                    let inByteArray = [|0uy;255uy;254uy|]
                    run _1d inByteArray
                    check inByteArray [|0uy;255uy;254uy|]

                // test fail on Intel platform:
                // Actual: [1uy, 255uy, 255uy]
                ptestCase "Byte and float 2" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<byte>) ->
                                if range.GlobalID0 = 0
                                then
                                    buf.[0] <- byte ((float buf.[0]) + 1.0)
                                    buf.[1] <- byte ((float buf.[1]) + 1.0)
                                    buf.[2] <- byte ((float buf.[2]) + 1.0)
                        @>

                    let run,check = checkResult command
                    let inByteArray = [|0uy;255uy;254uy|]
                    run _1d inByteArray
                    check inByteArray [|1uy;0uy;255uy|]

                // test failed on Intel platform:
                // Actual : [1uy, 1uy, 1uy]
                ptestCase "Byte and float in condition" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<byte>) ->
                                if range.GlobalID0 = 0
                                then
                                    let x = if true then buf.[0] + 1uy else buf.[0] + 1uy
                                    buf.[0] <- x
                                    let y = if true then buf.[1] + 1uy else buf.[1] + 1uy
                                    buf.[1] <- y
                                    let z = if true then buf.[2] + 1uy else buf.[2] + 1uy
                                    buf.[2] <- z
                        @>

                    let run,check = checkResult command
                    let inByteArray = [|0uy;255uy;254uy|]
                    run _1d inByteArray
                    check inByteArray [|1uy;0uy;255uy|]

                // test failed on Intel platform due to exception
                ptestCase "Byte and float in condition 2" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<byte>) ->
                                if range.GlobalID0 = 0
                                then
                                    let x =
                                        if true
                                        then
                                            let g = 1uy
                                            buf.[0] + g
                                        else buf.[0] + 1uy
                                    buf.[0] <- x
                                    let y =
                                        if true
                                        then
                                            let g = 1uy
                                            buf.[1] + g
                                        else buf.[1] + 1uy
                                    buf.[1] <- y
                                    let z =
                                        if true
                                        then
                                            let g = 1uy
                                            buf.[2] + g
                                        else buf.[2] + 1uy
                                    buf.[2] <- z
                        @>

                    let run,check = checkResult command
                    let inByteArray = [|0uy;255uy;254uy|]
                    run _1d inByteArray
                    check inByteArray [|1uy;0uy;255uy|]
          ]


    let bindingTests =
        testList "Bindings tests"
            [
                testCase "Bindings. Simple." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let x = 1
                                buf.[0] <- x
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|1;1;2;3|]

                testCase "Bindings. Sequential bindings." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let x = 1
                                let y = x + 1
                                buf.[0] <- y
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|2;1;2;3|]

                testCase "Bindings. Binding in IF." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                if 2 = 0
                                then
                                    let x = 1
                                    buf.[0] <- x
                                else
                                    let i = 2
                                    buf.[0] <- i
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|2;1;2;3|]

                testCase "Bindings. Binding in FOR." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                for i in 0..3 do
                                    let x = i * i
                                    buf.[i] <- x
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|0;1;4;9|]

                testCase "Bindings. Binding in WHILE." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                while buf.[0] < 5 do
                                    let x = buf.[0] + 1
                                    buf.[0] <- x * x
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|25;1;2;3|]
            ]

    let operatorsAndMathFunctionsTests =
        let testOpGen testCase
          (name: string)
          (binop: Expr<'a -> 'a -> 'a>)
          (xs: array<'a>)
          (ys: array<'a>)
          (expected: array<'a>) =
            testCase name <| fun _ ->
                let command =
                    <@
                        fun (range: _1D) (xs: array<'a>) (ys: array<'a>) (zs: array<'a>) ->
                            let i = range.GlobalID0
                            zs.[i] <- (%binop) xs.[i] ys.[i]
                    @>

                let range = (_1D <| Array.length expected)
                let zs = Array.zeroCreate <| Array.length expected

                let eval = opencl {
                    do! RunCommand command (fun binder -> binder range xs ys zs)
                    return! ToHost zs
                }

                let ctx = OpenCLEvaluationContext()
                let output = ctx.RunSync eval
                Expect.sequenceEqual output expected ":("

        testList "Operators and math functions tests"
            [
                testOpGen testCase "Boolean or 1." <@ (||) @>
                    [|true; false; false; false|]
                    [|false; true; true; true|]
                    [|true; true; true; true|]

                testOpGen testCase "Boolean or 2." <@ (||) @>
                    [|true; false|]
                    [|false; true|]
                    [|true; true|]

                testOpGen testCase "Boolean and 1." <@ (&&) @>
                    [|true; false; false; false|]
                    [|true; false; true; true|]
                    [|true; false; false; false|]

                testOpGen testCase "Binop plus 1." <@ (+) @>
                    [|1; 2; 3; 4|]
                    [|5; 6; 7; 8|]
                    [|6; 8; 10; 12|]

                // Failed: due to precision
                ptestCase "Math sin." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<float>) ->
                                let i = range.GlobalID0
                                buf.[i] <- System.Math.Sin (float buf.[i])
                        @>

                    let run,check = checkResult command
                    let inA = [|0.0;1.0;2.0;3.0|]
                    run _1d inA
                    check inA (inA |> Array.map System.Math.Sin)  //[|0.0; 0.841471; 0.9092974; 0.14112|]
            ]

    let pipeTests =
        ptestList "Pipe tests"
            [
                 // Lambda is not supported.
                ptestCase "Forward pipe." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                buf.[0] <- (1.25f |> int)
                        @>
                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|1;1;2;3|]

                // Lambda is not supported.
                ptestCase "Backward pipe." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                buf.[0] <- int <| 1.25f + 2.34f
                        @>
                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|3;1;2;3|]
            ]

    let controlFlowTests =
        testList "Control flow tests"
            [
                testCase "Control flow. If Then." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                if 0 = 2 then buf.[0] <- 42
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|0;1;2;3|]

                testCase "Control flow. If Then Else." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                if 0 = 2 then buf.[0] <- 1 else buf.[0] <- 2
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|2;1;2;3|]

                testCase "Control flow. For Integer Loop." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                for i in 1..3 do
                                    buf.[i] <- 0
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|0;0;0;0|]

                testCase "Control flow. WHILE loop simple test." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                while buf.[0] < 5 do
                                    buf.[0] <- buf.[0] + 1
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|5;1;2;3|]

                testCase "Control flow. WHILE in FOR." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                for i in 0..3 do
                                    while buf.[i] < 10 do
                                        buf.[i] <- buf.[i] * buf.[i] + 1
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|26;26;26;10|]
            ]

    let kernelArgumentsTests =
        testList "Kernel arguments tests"
            [
                testCase "Kernel arguments. Simple 1D." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let i = range.GlobalID0
                                buf.[i] <- i + i
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|0;2;4;6|]

                testCase "Kernel arguments. Simple 1D with copy." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (inBuf:array<int>) (outBuf:array<int>) ->
                                let i = range.GlobalID0
                                outBuf.[i] <- inBuf.[i]
                        @>

                    let run,check = checkResult command
                    let outA = [|0;0;0;0|]
                    run _1d intInArr outA
                    check outA [|0;1;2;3|]

                testCase "Kernel arguments. Simple 1D float." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<float32>) ->
                                let i = range.GlobalID0
                                buf.[i] <- buf.[i] * buf.[i]
                        @>

                    let run,check = checkResult command
                    run _1d float32Arr
                    check float32Arr [|0.0f;1.0f;4.0f;9.0f|]

                testCase "Kernel arguments. Int as arg." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) x (buf:array<int>) ->
                                let i = range.GlobalID0
                                buf.[i] <- x + x
                        @>

                    let run,check = checkResult command
                    run _1d 2 intInArr
                    check intInArr [|4;4;4;4|]

                testCase "Kernel arguments. Sequential commands over single buffer." <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) i x (buf:array<int>) ->
                                buf.[i] <- x + x
                        @>

                    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
                    let _, kernelPrepareF, kernelRunF = provider.Compile command

                    kernelPrepareF _1d 0 2 intInArr
                    commandQueue.Add(kernelRunF()) |> ignore

                    kernelPrepareF _1d 2 2 intInArr
                    commandQueue.Add(kernelRunF()) |> ignore
                    commandQueue.Finish() |> ignore

                    let r = Array.zeroCreate intInArr.Length
                    commandQueue.Add(intInArr.ToHost(provider, r)).Finish() |> ignore
                    commandQueue.Dispose()
                    provider.CloseAllBuffers()

                    let expected = [|4;1;4;3|]
                    Expect.sequenceEqual expected r "Arrays should be equals"
            ]

    let quotationInjectionTests =
        testList "Quotation injection tests"
            [
                testCase "Quotations injections.  Quotations injections 1." <| fun _ ->
                    let myF = <@ fun x -> x * x @>

                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                buf.[0] <- (%myF) 2
                                buf.[1] <- (%myF) 4
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|4;16;2;3|]

                testCase "Quotations injections. Quotations injections 2." <| fun _ ->
                    let myF = <@ fun x y -> y - x @>

                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                buf.[0] <- (%myF) 2 5
                                buf.[1] <- (%myF) 4 9
                        @>

                    let run,check = checkResult command
                    run _1d intInArr
                    check intInArr [|3;5;2;3|]
            ]

    let localMemTests =
        testList "Local memory tests"
            [
                // TODO: pointers to local data must be local too.
                testCase "Local int. Work item counting" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (output: array<int>) ->
                                let globalID = range.GlobalID0
                                let mutable x = local ()

                                if globalID = 0 then
                                    x <- 0
                                x <!+ 1
                                if globalID = 0 then
                                    output.[0] <- x
                        @>

                    let binder, check = checkResult command
                    let input = [|0|]
                    let range = _1D(5, 5)

                    binder range input
                    check input [|5|]

                testCase "Local array. Test 1" <| fun _ ->
                    let localWorkSize = 5
                    let globalWorkSize = 15

                    let command =
                        <@
                            fun (range:_1D) (input: array<int>) (output: array<int>) ->
                                let local_buf: array<int> = localArray localWorkSize

                                local_buf.[range.LocalID0] <- range.LocalID0
                                output.[range.GlobalID0] <- local_buf.[(range.LocalID0 + 1) % localWorkSize]
                        @>

                    let kernelPrepare, check = checkResult command

                    let range = _1D(globalWorkSize, localWorkSize)

                    let input = Array.zeroCreate globalWorkSize
                    let output = Array.zeroCreate globalWorkSize

                    kernelPrepare range input output
                    let expected = [| for x in 1..localWorkSize -> x % localWorkSize |]
                                   |> Array.replicate (globalWorkSize / localWorkSize)
                                   |> Array.concat

                    check output expected

                testCase "Local array. Test 2" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int64>) ->
                                let local_buf = localArray 42
                                local_buf.[0] <! 1L
                                buf.[0] <- local_buf.[0]
                        @>

                    let run,check = checkResult command
                    let initInArr = [|0L; 1L; 2L; 3L|]
                    run _1d initInArr
                    check initInArr [|1L; 1L; 2L; 3L|]
            ]

    let letTransformationTests =
        testList "Let Transformation Tests"
            [
                testCase "Template Let Transformation Test 0" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f = 3
                                buf.[0] <- f
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|3; 1; 2; 3|]

                testCase "Template Let Transformation Test 1" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let x = 4
                                let f =
                                    let x = 3
                                    x
                                buf.[0] <- x + f
                        @>
                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|7; 1; 2; 3|]

                testCase "Template Let Transformation Test 1.2" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f y =
                                    let x c b = b + c + 4 + y
                                    x 2 3
                                buf.[0] <- f 1
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|10; 1; 2; 3|]

                testCase "Template Let Transformation Test 2" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f =
                                    let x =
                                        let y = 3
                                        y
                                    x
                                buf.[0] <- f
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|3; 1; 2; 3|]

                testCase "Template Let Transformation Test 3" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f =
                                    let f = 5
                                    f
                                buf.[0] <- f
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|5; 1; 2; 3|]


                testCase "Template Let Transformation Test 4" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f =
                                    let f =
                                        let f = 5
                                        f
                                    f
                                buf.[0] <- f
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|5; 1; 2; 3|]

                testCase "Template Let Transformation Test 5" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f a b =
                                    let x y z = y + z
                                    x a b
                                buf.[0] <- f 1 7
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|8; 1; 2; 3|]

                testCase "Template Let Transformation Test 6" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f x y =
                                    let x = x
                                    x + y
                                buf.[0] <- f 7 8
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|15; 1; 2; 3|]

                testCase "Template Let Transformation Test 7" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f y =
                                    let x y = 6 - y
                                    x y
                                buf.[0] <- f 7
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|-1; 1; 2; 3|]

                testCase "Template Let Transformation Test 8" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (m:array<int>) ->
                                let p = m.[0]
                                let x n =
                                    let l = m.[3]
                                    let g k = k + m.[0] + m.[1]
                                    let r =
                                        let y a =
                                            let x = 5 - n + (g 4)
                                            let z t = m.[2] + a - t
                                            z (a + x + l)
                                        y 6
                                    r + m.[3]
                                if range.GlobalID0 = 0
                                then m.[0] <- x 7
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|-1; 1; 2; 3|]

                testCase "Template Let Transformation Test 9" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let x n =
                                    let r = 8
                                    let h = r + n
                                    h
                                buf.[0] <- x 9
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|17; 1; 2; 3|]

                testCase "Template Let Transformation Test 10" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let p = 9
                                let x n b =
                                    let t = 0
                                    n + b + t
                                buf.[0] <- x 7 9
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|16; 1; 2; 3|]

                testCase "Template Let Transformation Test 11" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let p = 1
                                let m =
                                    let r (l:int) = l
                                    r 9
                                let z (k:int) = k
                                buf.[0] <- m
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|9; 1; 2; 3|]

                testCase "Template Let Transformation Test 12" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f x y =
                                    let y = y
                                    let y = y
                                    let g x m = m + x
                                    g x y
                                buf.[0] <- f 1 7
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|8; 1; 2; 3|]

                testCase "Template Let Transformation Test 13" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f y =
                                    let y = y
                                    let y = y
                                    let g (m:int) = m
                                    g y
                                buf.[0] <- f 7
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|7; 1; 2; 3|]

                testCase "Template Let Transformation Test 14" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f y =
                                    let y = y
                                    let y = y
                                    let g (m:int) =
                                        let g r t = r + y - t
                                        let n o = o - (g y 2)
                                        n 5
                                    g y
                                let z y = y - 2
                                buf.[0] <- f (z 7)
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|-3; 1; 2; 3|]

                testCase "Template Let Transformation Test 15" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f y =
                                    let Argi index =
                                        if index = 0
                                        then buf.[1]
                                        else buf.[2]
                                    Argi y
                                buf.[0] <- f 0
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|1; 1; 2; 3|]

                testCase "Template Let Transformation Test 16" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let f y =
                                    if y = 0
                                    then
                                        let z (a:int) = a
                                        z 9
                                    else buf.[2]
                                buf.[0] <- f 0
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|9; 1; 2; 3|]

                testCase "Template Let Transformation Test 17" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                if range.GlobalID0 = 0
                                then
                                    let f y =
                                        let g = buf.[1] + 1
                                        y + g
                                    for i in 0..3 do
                                       buf.[i] <- f i
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|2; 3; 6; 7|]

                testCase "Template Let Transformation Test 18" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                for i in 0..3 do
                                    let f =
                                        let g = buf.[1] + 1
                                        i + g
                                    if range.GlobalID0 = 0
                                    then buf.[i] <- f
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|2; 3; 6; 7|]

                testCase "Template Let Transformation Test 19" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                if range.GlobalID0 = 0
                                then
                                    for i in 0..3 do
                                        let f x =
                                            let g = buf.[1] + x
                                            i + g
                                        buf.[i] <- f 1
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|2; 3; 6; 7|]

                // TODO: perform range (1D, 2D, 3D) erasure when range is lifted.
                ptestCase "Template Let Transformation Test 20" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (m:array<int>) ->
                                let f x =
                                    range.GlobalID0 + x
                                m.[0] <- f 2
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|2; 3; 6; 7|]
            ]

    let letQuotationTransformerSystemTests =
        testList "Let Transformation Tests Mutable Vars"
            [
                testCase "Test 0" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (buf:array<int>) ->
                                let mutable x = 1
                                let f y =
                                    x <- y
                                f 10
                                buf.[0] <- x
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|10; 1; 2; 3|]

                testCase "Test 1" <| fun _ ->
                    let command =
                        <@
                            fun (range: _1D) (buf: array<int>) ->
                                let mutable x = 1
                                let f y =
                                    x <- x + y
                                f 10
                                buf.[0] <- x
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|11; 1; 2; 3|]

                testCase "Test 2" <| fun _ ->
                    let command =
                        <@
                            fun (range: _1D) (arr: array<int>) ->
                                let f x =
                                    let g y = y + 1
                                    g x
                                arr.[0] <- f 2
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|3; 1; 2; 3|]

                testCase "Test 3" <| fun _ ->
                    let command =
                        <@
                            fun (range: _1D) (arr: array<int>)->
                                let f x =
                                    let g y =
                                        y + x
                                    g (x + 1)
                                arr.[0] <- f 2
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|5; 1; 2; 3|]

                testCase "Test 4" <| fun _ ->
                    let command =
                        <@
                            fun (range: _1D) (arr: array<int>) ->
                                let gid = range.GlobalID0
                                let x =
                                    let mutable y = 0

                                    let addToY x =
                                        y <- y + x

                                    for i in 0..5 do
                                        addToY arr.[gid]
                                    y
                                arr.[gid] <- x
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|0; 6; 12; 18|]

                testCase "Test 5" <| fun _ ->
                    let command =
                        <@
                            fun (range: _1D) (arr: array<int>) ->
                                let gid = range.GlobalID0

                                let mutable x =
                                    if 0 > 1 then 2 else 3

                                let mutable y =
                                    for i in 0..4 do
                                        x <- x + 1
                                    x + 1

                                let z =
                                    x + y

                                let f () =
                                    arr.[gid] <- x + y + z
                                f ()
                        @>

                    let run, check = checkResult command
                    run _1d intInArr
                    check intInArr [|34; 34; 34; 34|]


            ]

    testList "System tests with running kernels"
        [
            letTransformationTests
            letQuotationTransformerSystemTests
            atomicsTests
            arrayItemSetTests
            typeCastingTests
            bindingTests
            operatorsAndMathFunctionsTests
            pipeTests
            controlFlowTests
            kernelArgumentsTests
            quotationInjectionTests
            localMemTests
        ]
    |> (fun x -> Expecto.Sequenced (Expecto.SequenceMethod.Synchronous, x))

(*

    [<Test>]
    member this.``Write buffer``() =
        let command =
            <@
                fun (range:_1D) (buf:array<byte>) ->
                    if range.GlobalID0 = 0
                    then
                        buf.[0] <- buf.[0] + 1uy
                        buf.[1] <- buf.[1] + 1uy
                        buf.[2] <- buf.[2] + 1uy
            @>
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        let inArray = [|1uy;2uy;3uy|]
        kernelPrepareF _1d inArray
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Add(inArray.ToHost provider).Finish()
        let expected = [|2uy;3uy;4uy|]
        Assert.AreEqual(expected, inArray)
        inArray.[0] <- 5uy
        commandQueue.Add(inArray.ToGpu provider) |> ignore
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Add(inArray.ToHost provider).Finish()
        let expected = [|6uy;4uy;5uy|]
        Assert.AreEqual(expected, inArray)
        commandQueue.Dispose()
        provider.CloseAllBuffers()

    [<Test>]
    member this.``Buffers initialisation``() =
        let command =
            <@
                fun (range:_1D) (buf:array<byte>) ->
                    if range.GlobalID0 = 0
                    then
                        buf.[0] <- buf.[0] + 1uy
                        buf.[1] <- buf.[1] + 1uy
                        buf.[2] <- buf.[2] + 1uy
            @>
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        let inArray = [|1uy;2uy;3uy|]
        kernelPrepareF _1d inArray
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
        let _ = commandQueue.Add(inArray.ToGpu(provider,[|2uy;3uy;4uy|]))
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Add(inArray.ToHost provider).Finish()
        let expected = [|3uy;4uy;5uy|]
        Assert.AreEqual(expected, inArray)
        commandQueue.Dispose()
        provider.CloseAllBuffers()

    [<Test>]
    member this.``While with preheader.``() =
        let command =
            <@
                fun (rng:_1D) ->
                    let mutable parent = 10s

                    while parent > 0s do
                        let currentTemplate = 10s
                        parent <- parent - 1s

            @>

        let kernel,kernelPrepare, kernelRun = provider.Compile command
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
        kernelPrepare _1d
        try
            commandQueue.Add(kernelRun()).Finish()
            |> ignore
            commandQueue.Dispose()
        with e ->
            commandQueue.Dispose()
            Assert.Fail e.Message

    [<Test>]
    member this.``Double on GPU.``() =
        let command =
            <@
                fun (r:_2D) ->
                    let x = r.GlobalID0
                    let y = r.GlobalID1
                    let scaling = 0.5
                    let b = ref 0.0
                    let size : float = 100.0
                    let fx = float x / size * scaling + float -1.5
                    b := 0.0
            @>

        let kernel,kernelPrepare, kernelRun = provider.Compile command
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
        kernelPrepare _2d
        try
            commandQueue.Add(kernelRun()).Finish()
            |> ignore
            commandQueue.Dispose()
        with e ->
            commandQueue.Dispose()
            Assert.Fail e.Message

    [<Test>]
    member this.``Simple seq of struct.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<TestStruct>) ->
                    if range.GlobalID0 = 0
                    then
                        let b = buf.[0]
                        buf.[0] <- buf.[1]
                        buf.[1] <- b
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0);new TestStruct(3, 4.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(3, 4.0); new TestStruct(1, 2.0)|]

    [<Ignore("Strange test...")>]
    [<Test>]
    member this.``Simple seq of struct changes.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0] <- new TestStruct(5,6.0)
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0);new TestStruct(3, 4.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(3, 4.0); new TestStruct(1, 2.0)|]

    [<Test>]
    member this.``Simple seq of struct prop set.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0].x <- 5
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(5, 2.0)|]

    [<Test>]
    member this.``Simple seq of struct prop get.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0].x <- buf.[1].x + 1
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0);new TestStruct(3, 4.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(4, 2.0); new TestStruct(3, 4.0)|]

    [<Test>]
    member this.``Atomic max.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMaxR buf.[0] 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray [|0|]
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic max 2.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMaxR buf.[0] 1
            @>
        let run,check = checkResult command
        let inByteArray = [|2|]
        run _1d inByteArray [|0|]
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic min.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMinR buf.[0] 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray [|0|]
        check inByteArray [|1|]

    [<Test>]
    member this.``Atomic min 2.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMinR buf.[0] 1
            @>
        let run,check = checkResult command
        let inByteArray = [|2|]
        run _1d inByteArray [|0|]
        check inByteArray [|1|]

    [<Test>]
    member this.``Atomic exchange.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- buf.[0] <!>  2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray [|0|]
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic exchange 2.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    buf.[0] <! 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic decr return.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    buf.[1] <- aDecrR buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|0;0;0;0|]
        run _1d inByteArray
        check inByteArray [|-4;-3;0;0|]

    [<Test>]
    member this.``Atomic decr.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    aDecr buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|0;0;0;0|]
        run _1d inByteArray
        check inByteArray [|-4;0;0;0|]

    [<Test>]
    member this.``Atomic incr return.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    buf.[1] <- aIncrR buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|1;2;0;0|]
        run _1d inByteArray
        check inByteArray [|5;4;0;0|]

    [<Test>]
    member this.``Atomic incr.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    aIncr buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|0;0;0;0|]
        run _1d inByteArray
        check inByteArray [|4;0;0;0|]

    [<Test>]
    member this.``Atomic compare exchange return.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    buf.[1] <- aCompExchR buf.[0] 1 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1;0;0;0|]
        run _1d inByteArray
        check inByteArray [|2;2;0;0|]

    [<Test>]
    member this.``Atomic compare exchange.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    aCompExch buf.[0] 1 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1;0;0;0|]
        run _1d inByteArray
        check inByteArray [|2;0;0;0|]

    [<Test>]
    member this.``Atomic compare exchange 2.``() =
        let command =
            <@
                fun (range:_1D) (buf:array<_>) ->
                    aCompExch buf.[0] 1 2
            @>
        let run,check = checkResult command
        let inByteArray = [|3;0;0;0|]
        run _1d inByteArray
        check inByteArray [|3;0;0;0|]


    [<Test>]
    member this.``Template Let Transformation Test 9``() =
        let command =
            <@
                fun (range:_1D) (buf:array<int>) ->
                        let x n =
                            let mutable r = 8
                            let mutable h = r + n
                            h
                        buf.[0] <- x 9
            @>

        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|17;1;2;3|]

    [<Test>]
    member this.``createStartStoreKernel``() =
        let command =
                <@ fun (r:_2D) (devStore:array<_>) (scaleExp) (scaleM:int) (scaleVar:int) ->
                        let column = r.GlobalID0
                        let row = r.GlobalID1

                        if row < scaleExp && column < scaleM
                        then
                            if row < scaleVar
                            then
                                if column % scaleM = 0
                                then devStore.[row*scaleM + column] <- 1
                                else devStore.[row*scaleM + column] <- -1
                            elif column = 0
                            then devStore.[row*scaleM + column] <- 2
                            elif column = 1
                            then devStore.[row*scaleM + column] <- row - scaleVar + 1
                            else devStore.[row*scaleM + column] <- -1
                @>

        let initStore,check = checkResult command
        let intArr = Array.zeroCreate 45
        initStore (new _2D(5, 9)) intArr 9 5 6
        check intArr   [|1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         2; 1;-1;-1;-1;
                         2; 2;-1;-1;-1;
                         2; 3;-1;-1;-1
        |]

    [<Test>]
    member this.twoFun() =
        let command =
                <@ fun (r:_1D) (devStore:array<int>) ->
                        let x y =
                            devStore.[0] <- devStore.[0] + 1
                            y + 2
                        if r.GlobalID0 = 0
                        then devStore.[1] <- x 9
                @>

        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|1; 11; 2; 3|]


    [<Test>]
    member this.EigenCFA() =
        let command =
                <@ fun (r:_2D) (devStore:array<_>) (scaleExp) (scaleM:int) (scaleVar:int) ->
                        let column = r.GlobalID0
                        let row = r.GlobalID1

                        if row < scaleExp && column < scaleM
                        then
                            if row < scaleVar
                            then
                                if column % scaleM = 0
                                then devStore.[row*scaleM + column] <- 1
                                else devStore.[row*scaleM + column] <- -1
                            elif column = 0
                            then devStore.[row*scaleM + column] <- 2
                            elif column = 1
                            then devStore.[row*scaleM + column] <- row - scaleVar + 1
                            else devStore.[row*scaleM + column] <- -1
                @>

        let qEigenCFA =
                <@ fun (r:_2D)
                    (devFun:array<_>)
                    (devArg1:array<_>)
                    (devArg2:array<_>)
                    (devStore:array<_>)
                    (devRep:array<_>)
                    devScaleM
                    devScaleCall
                    devScaleLam ->
                       let column = r.GlobalID0
                       let row = r.GlobalID1
                       if column < devScaleCall && row < 2
                       then
                            let numCall = column
                            let Argi index =
                                if index = 0
                                then devArg1.[numCall]
                                else devArg2.[numCall]
                            let L index = devStore.[devFun.[numCall]*devScaleM + index]
                            let Li index = devStore.[(Argi row)*devScaleM + index]
                            let rowStore row column = devStore.[row*devScaleM + column]
                            let vL j =
                                if row = 0
                                then (L j) - 1
                                else (L j) - 1 + devScaleLam
                            for j in 1 .. ((L 0) - 1) do
                                for k in 1 .. ((Li 0) - 1) do
                                    let mutable isAdd = 1
                                    let addVar = Li k
                                    for i in 1 .. (rowStore (vL j) 0) - 1 do
                                        if rowStore (vL j) i = addVar
                                        then isAdd <- 0
                                    if isAdd > 0 then
                                        devRep.[0] <- devRep.[0] + 1
                                        let tail = (rowStore (vL j) 0)
                                        devStore.[(vL j)*devScaleM] <- devStore.[(vL j)*devScaleM] + 1
                                        devStore.[(vL j)*devScaleM + tail] <- addVar
                @>
//        let initStore,check = checkResult command
//        let intArr = Array.zeroCreate 45
//        initStore (new _2D(5, 9)) intArr 9 5 6

        let intArr =  [|1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        2; 1;-1;-1;-1;
                        2; 2;-1;-1;-1;
                        2; 3;-1;-1;-1
                        |]

        let kernel,kernelPrepareF, kernelRunF = provider.Compile qEigenCFA
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

//        let EigenCFAStart,checkCFA = checkResult qEigenCFA

        let mutable rep = -1
        let mutable curRep = -2
        let mutable iter = 1

        let Fun = [|5;1;0;8|]
        let Arg1 = [|5;4;3;7|]
        let Arg2 = [|5;4;3;6|]
        let repArray = Array.zeroCreate 1

        while(rep <> curRep) do
            iter <- iter + 1
            rep <- curRep
            let EigenCFA =
                kernelPrepareF (new _2D(4, 2)) Fun Arg1 Arg2 intArr repArray 5 4 3
                let cq = commandQueue.Add(kernelRunF()).Finish()
                let r = Array.zeroCreate 1
                let cq2 = commandQueue.Add(repArray.ToHost(provider,r)).Finish()
                printf "%A\n" r
                r
            let a = EigenCFA
            curRep <- a.[0]


        let expectedResult =  [|2; 1;-1;-1;-1;
                                1;-1;-1;-1;-1;
                                2; 2;-1;-1;-1;
                                2; 1;-1;-1;-1;
                                1;-1;-1;-1;-1;
                                2; 1;-1;-1;-1;
                                2; 1;-1;-1;-1;
                                2; 2;-1;-1;-1;
                                2; 3;-1;-1;-1
                                |]

        let cq = commandQueue.Add(kernelRunF()).Finish()
        let r = Array.zeroCreate 45
        let cq2 = commandQueue.Add(intArr.ToHost(provider,r)).Finish()

        Assert.AreEqual(expectedResult, r)

        provider.CloseAllBuffers()

    [<Test>]
    [<Ignore("Image 2D not finished")>]
    member this.``Checking of Image2D``() =
        let command =
            <@
                fun (range:_1D) (img:Image2D<ARGB<Float>>) (a:array<_>) ->
                    a.[0] <- 1
            @>
        let CLimg = new Image2D<_>(provider, Operations.ReadOnly, true, 10, 10, -1)
        let run,check = checkResult command
        run _1d CLimg intInArr
        check intInArr [|1;3;6;7|]



let x =
    let d = ref 0
    fun y ->
        let r = !d
        d := !d + y
        r*)
