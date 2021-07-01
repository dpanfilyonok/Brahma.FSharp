module Brahma.FSharp.Tests.DevTests

open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.WorkflowBuilder
open Expecto
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Printer.AST
open FSharp.Quotations

type TestStruct =
    val mutable x: int
    val mutable y: float
    new (x,y) = {x=x; y=y}

let deviceType = DeviceType.Gpu
let platformName = "Intel*"

let provider =
    try ComputeProvider.Create(platformName, deviceType)
    with ex -> failwith ex.Message

let context = OpenCLEvaluationContext(platformName, deviceType)

let openclCompile (command: Quotations.Expr<('a -> 'b)>): string =
        let code = ref ""
        provider.Compile(command, _outCode=code) |> ignore
        !code

let openclTranslate (expr: Expr) =
    let translator = FSQuotationToOpenCLTranslator()
    let ast, methods = translator.Translate expr []
    Print ast

[<Tests>]
let devTests =
    testCase "devtest" <| fun _ ->
        printf ":)"
