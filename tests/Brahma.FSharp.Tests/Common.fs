namespace Brahma.FSharp.Tests

open System.IO
open Brahma.OpenCL
open Expecto
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.WorkflowBuilder
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Printer.AST
open FSharp.Quotations

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
    let context = OpenCLEvaluationContext()

    let finalize f =
        try
            f ()
        finally
            context.Provider.CloseAllBuffers()

    let getValidGlobalSize wgSize neededSize = (neededSize + wgSize - 1) / wgSize * wgSize

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

    let platformMessage (provider: ComputeProvider) testName = printfn "Run %s on %A" testName provider

    let openclCompile (command: Expr<('a -> 'b)>) =
        let code = ref ""
        context.Provider.Compile(command, outCode = code) |> ignore
        !code

    let openclTranslate (expr: Expr) =
        let translator = FSQuotationToOpenCLTranslator()
        let (ast, methods) = translator.Translate(expr, [])
        print ast
