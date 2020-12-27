module Brahma.FSharp.Tests.Workflow

open FSharp.Quotations

open Expecto
open OpenCL.Net

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open Brahma.FSharp.OpenCL.WorkflowBuilder.Basic

[<Tests>]
let WorkflowTests =
    let ctx = OpenCLEvaluationContext()
    printfn "Running workflow tests on:\n%A" ctx.Provider

    let eqMsg = "Values should be equal"

    let GpuMap (f: Expr<'a -> 'b>) (input : array<'a>) =
        opencl {
            let res = Array.zeroCreate input.Length

            let code =
                <@
                    fun (range : _1D) (input : array<'a>) (output : array<'b>) ->
                        let idx = range.GlobalID0
                        output.[idx] <- (%f) input.[idx]
                @>

            let binder kernelP =
                let range = _1D <| input.Length
                kernelP range input res

            do! RunCommand code binder
            return res
        }

    let bindTests =
        testList "Simple bind tests"
            [
                testCase "Test 1" <| fun _ ->
                    let xs = [|1; 2; 3; 4|]
                    let workflow =
                        opencl {
                            let! ys = GpuMap <@ fun x -> x * x + 10 @> xs
                            let! zs = GpuMap <@ fun x -> x + 1 @> ys
                            return! ToHost zs
                        }
                    let output = ctx.RunSync workflow
                    Expect.equal output [|12; 15; 20; 27|] eqMsg
            ]

    let loopTests =
        testList "Loop tests"
            [
                testCase "While. Test 1. Without evaluation" <| fun _ ->

                    let mutable log: int list = []
                    let workflow = opencl {
                        let mutable i = 0
                        log <- i :: log

                        while i < 10 do
                            i <- i + 1
                            log <- i :: log
                    }
                    Expect.equal log [] "Delay should not allow any computations
                                         before evaluation started"
                    ctx.RunSync workflow
                    Expect.equal log [10..-1..0] eqMsg

                testCase "While. Test 2. Simple evaluation" <| fun _ ->
                    let mutable xs = [|1; 2; 3; 4; 5; 6; 7; 8|]
                    let iters = 5
                    let expected = Array.map (fun x -> pown 2 iters * x) xs

                    let workflow = opencl {
                        let f = <@ fun x -> x * 2 @>

                        let mutable i = 0
                        while i < iters do
                            let! res = GpuMap f xs
                            xs <- res
                            i <- i + 1

                        return! ToHost xs
                    }
                    let output = ctx.RunSync workflow
                    Expect.equal output expected eqMsg
            ]

    let asyncRunTests =
        testList "Tests of async workflow"
            [
                testCase "Test 1" <| fun _ ->
                    let command =
                        <@
                            fun (range:_1D) (xs:array<int>) ->
                                xs.[range.GlobalID0] <- range.LocalID0
                        @>

                    let workflow globalWorkSize localWorkSize =
                        opencl {
                            let xs = Array.zeroCreate globalWorkSize
                            let range = _1D(globalWorkSize, localWorkSize)

                            let binder prepareF =
                                prepareF range xs

                            do! RunCommand command binder
                            return! ToHost xs
                        }

                    let get_result = ctx.RunAsync <| workflow (32*10000) 32

                    do Some |> ignore

                    let result = get_result ()
                    let expected = Array.replicate 10000 [|0..31|] |> Array.concat
                    Expect.equal result expected eqMsg
            ]

    testList "System tests with running kernels"
        [
            bindTests
            loopTests
            asyncRunTests
        ]
    |> (fun x -> Expecto.Sequenced (Expecto.SequenceMethod.Synchronous, x))
