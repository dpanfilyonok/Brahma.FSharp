module Workflow

open FSharp.Quotations
open Expecto
open System.Collections.Generic
//open Brahma.OpenCL
//open Brahma.FSharp.OpenCL.WorkflowBuilder

//TODO: redesign me

(*let ctx = OpenCLEvaluationContext()

let eqMsg = "Values should be equal"

let gpuMap (f: Expr<'a -> 'b>) (input: array<'a>) =
    opencl {
        let res = Array.zeroCreate input.Length

        let code =
            <@ fun (range: Range1D) (input: array<'a>) (output: array<'b>) ->
                let idx = range.GlobalID0
                output.[idx] <- (%f) input.[idx] @>

        let binder kernelP =
            let range = Range1D <| input.Length
            kernelP range input res

        do! runCommand code binder
        return res
    }

let bindTests = testList "Simple bind tests" [
    testCase "Test 1" <| fun _ ->
        let xs = [| 1; 2; 3; 4 |]

        let workflow =
            opencl {
                let! ys = gpuMap <@ fun x -> x * x + 10 @> xs
                let! zs = gpuMap <@ fun x -> x + 1 @> ys
                return! toHost zs
            }

        let output = ctx.RunSync workflow
        Expect.equal output [| 12; 15; 20; 27 |] eqMsg
]

let loopTests = testList "Loop tests" [
    testCase "While. Test 1. Without evaluation" <| fun _ ->
        let mutable log : int list = []

        let workflow =
            opencl {
                let mutable i = 0
                log <- i :: log

                while i < 10 do
                    i <- i + 1
                    log <- i :: log
            }

        Expect.equal log [] "Delay should prevent any computations before evaluation started"
        ctx.RunSync workflow
        Expect.equal log [ 10 .. -1 .. 0 ] eqMsg

    testCase "While. Test 2. Simple evaluation" <| fun _ ->
        let mutable xs = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let iters = 5
        let expected = Array.map (fun x -> pown 2 iters * x) xs

        let workflow =
            opencl {
                let f = <@ fun x -> x * 2 @>

                let mutable i = 0

                while i < iters do
                    let! res = gpuMap f xs
                    xs <- res
                    i <- i + 1

                return! toHost xs
            }

        let output = ctx.RunSync workflow
        Expect.equal output expected eqMsg

    testCase "While. Test 3. Do inside body of while loop" <| fun _ ->
        let xs : int array ref = ref [| 1; 2; 3; 4 |]

        let gpuMapInplace f (xs: int array ref) =
            opencl {
                let! res = gpuMap f !xs
                xs := res
            }

        let workflow =
            opencl {
                let mutable i = 0

                while i < 10 do
                    do! gpuMapInplace <@ fun x -> x + 1 @> xs
                    i <- i + 1

                return! toHost !xs
            }

        let output = ctx.RunSync workflow
        Expect.equal output [| 11; 12; 13; 14 |] eqMsg

    testCase "For. Test 1. Without evaluation" <| fun _ ->
        let log = List<int>()

        let workflow =
            opencl {
                log.Add(0)

                for x in [ 1 .. 10 ] do
                    log.Add(x)
            }

        Expect.sequenceEqual log
        <| List<int>()
        <| "Delay should prevent any computations before evaluation started"

        ctx.RunSync workflow
        Expect.sequenceEqual log (List<int>([ 0 .. 10 ])) eqMsg

    testCase "For. Test 2. Simple evaluation" <| fun _ ->
        let workflow =
            opencl {
                let mutable xs = [| 1; 2; 3; 4 |]

                for y in [| 10; 20; 30 |] do
                    let! res = gpuMap <@ fun x -> x + y @> xs
                    xs <- res

                return! toHost xs
            }

        let output = ctx.RunSync workflow
        Expect.equal output [| 61; 62; 63; 64 |] eqMsg
]

let asyncRunTests = testList "Tests of async workflow" [
    ptestCase "Test 1" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (xs: array<int>) -> xs.[range.GlobalID0] <- range.LocalID0 @>

        let workflow globalWorkSize localWorkSize =
            opencl {
                let xs = Array.zeroCreate globalWorkSize
                let range = Range1D (globalWorkSize, localWorkSize)

                let binder prepareF = prepareF range xs

                do! runCommand command binder
                return! toHost xs
            }

        let getResult = ctx.RunAsync <| workflow (32 * 10000) 32
        do Some |> ignore

        let result = getResult ()
        let expected = Array.replicate 10000 [| 0 .. 31 |] |> Array.concat
        Expect.equal result expected eqMsg
]

let commonAPITests = testList "Tests of async workflow" [
    testCase "Test 1: ToHost non-gpu array" <| fun _ ->
        let eval =
            opencl {
                let input = [| 1, 2, 3, 4 |]

                return! toHost input
            }

        let res = ctx.RunSync eval
        Expect.equal res [| 1, 2, 3, 4 |] eqMsg
]

let tests =
    testList "System tests with running kernels" [
        bindTests
        loopTests
        asyncRunTests
        commonAPITests
    ]
    |> fun x -> Expecto.Sequenced(Synchronous, x)
*)
