module Brahma.FSharp.Tests.DevTests

open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open Expecto
open Brahma.FSharp.OpenCL
open Brahma.FSharp.OpenCL.Printer.AST
open FSharp.Quotations

type TestStruct =
    val mutable x: int
    val mutable y: float
    new (x,y) = {x=x; y=y}

let gpu =
    let deviceType = OpenCL.Net.DeviceType.Default
    let platformName = "Intel*"
    let devices = Device.getDevices platformName deviceType
    GPU(devices.[0])

//let context = OpenCLEvaluationContext(device_type=deviceType)

let opencl_compile (command: Quotations.Expr<('a -> 'b)>): string =
        let kernel = gpu.CreateKernel command
        let code = kernel.ClCode
        code

let opencl_translate (expr: Expr) =
    let translator = FSQuotationToOpenCLTranslator()
    let ast, methods = translator.Translate expr []
    Print ast

[<Tests>]
let dev_tests =
    testCase "devtest" <| fun _ ->
        printf ":)"
