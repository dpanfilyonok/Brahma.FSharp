module Brahma.FSharp.Tests.Workflow

open FSharp.Quotations

open Expecto
open OpenCL.Net

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Workflow.Workflow
open Brahma.FSharp.OpenCL.Workflow.BasicEvaluations

[<Tests>]
let WorkflowTests =
    let deviceType = DeviceType.Default
    let platformName = "*"

    let provider = ComputeProvider.Create(platformName, deviceType)
    let ctx = OpenCLContext provider

    let eqMsg = "Values should be equal"

    let mapWorkflowTests =
        let GpuMap (f: Expr<'a -> 'b>) (input : array<'a>) =
            evaluation {
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
                        evaluation {
                            let! ys = GpuMap <@ fun x -> x * x + 10 @> xs
                            let! zs = GpuMap <@ fun x -> x + 1 @> ys
                            return! ToHost zs
                        }
                    let output = ctx.RunSync workflow
                    Expect.equal output [|12; 15; 20; 27|] eqMsg
            ]


    testList "System tests with running kernels"
        [
            mapWorkflowTests
        ]
    |> (fun x -> Expecto.Sequenced (Expecto.SequenceMethod.Synchronous, x))
