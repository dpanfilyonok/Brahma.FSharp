module Brahma.FSharp.Tests.Workflow

open FSharp.Quotations

open Expecto
open OpenCL.Net

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Workflow.Evaluation
open Brahma.FSharp.OpenCL.Workflow.Basic

[<Tests>]
let WorkflowTests =
    let deviceType = DeviceType.Default
    let platformName = "*"

    let provider = ComputeProvider.Create(platformName, deviceType)
    let ctx = OpenCLContext provider

    let eqMsg = "Values should be equal"

    let mapWorkflowTests =
        let GpuMap (f: Expr<'a -> 'b>) (input : array<'a>) =
            opencl {
                let xs = Array.zeroCreate input.Length

                let code =
                    <@
                        fun (range : _1D) (input : array<'a>) (output : array<'b>) ->
                            let idx = range.GlobalID0
                            output.[idx] <- (%f) input.[idx]
                    @>

                let binder kernelP =
                    let range = _1D <| input.Length
                    kernelP range input xs

                do! RunCommand code binder
                return xs
            }

        testList "Tests of map workflow"
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

                    let get_result = ctx.RunAsync <| workflow (32*1000) 32

                    do Some |> ignore

                    let result = get_result ()
                    let expected = Array.replicate 1000 [|0..31|] |> Array.concat
                    Expect.equal result expected eqMsg
            ]

    testList "System tests with running kernels"
        [
            mapWorkflowTests
            asyncRunTests
        ]
    |> (fun x -> Expecto.Sequenced (Expecto.SequenceMethod.Synchronous, x))
