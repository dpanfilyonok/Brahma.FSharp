namespace Brahma.FSharp.Tests

open System.IO
open Brahma.FSharp.OpenCL
open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Printer.AST
open FSharp.Quotations

[<AutoOpen>]
module Common =
    let context =
        let deviceType = ClDeviceType.Default
        let platformName = ClPlatform.Any
        ClContext(platformName, deviceType)

    let defaultInArrayLength = 4
    let intInArr = [| 0 .. defaultInArrayLength - 1 |]
    let float32Arr = Array.init defaultInArrayLength float32
    let default1D = Range1D(defaultInArrayLength, 1)
    let default2D = Range2D(defaultInArrayLength, 1)

    let checkResult command (inArr: 'a[]) (expectedArr: 'a[]) =
        let actual =
            opencl {
                use! inBuf = ClArray.toDevice inArr
                do! runCommand command <| fun x ->
                    x default1D inBuf

                return! ClArray.toHost inBuf
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expectedArr "Arrays should be equals"

module CustomDatatypes =
    [<Struct>]
    type WrappedInt =
        val mutable InnerValue: int
        new(x) = { InnerValue = x }

        static member (+) (x: WrappedInt, y: WrappedInt) =
            WrappedInt(x.InnerValue + y.InnerValue)

        static member (-) (x: WrappedInt, y: WrappedInt) =
            WrappedInt(x.InnerValue - y.InnerValue)

module Utils =
    let filesAreEqual file1 file2 =
        let all1 =
            (File.ReadAllText file1)
                .Trim()
                .Replace("\r\n", "\n")

        let all2 =
            (File.ReadAllText file2)
                .Trim()
                .Replace("\r\n", "\n")

        Expect.equal all1 all2 "Files should be equals as strings"

    let openclCompile (command: Expr<('a -> 'b)>) =
        let kernel = context.CreateClProgram command
        kernel.Code

    let openclTranslate (expr: Expr) =
        let translator = FSQuotationToOpenCLTranslator(TranslatorOptions())
        let (ast, _) = translator.Translate expr
        print ast

    let openclTransformQuotation (expr: Expr) =
        let translator = FSQuotationToOpenCLTranslator(TranslatorOptions())
        translator.TransformQuotation expr
