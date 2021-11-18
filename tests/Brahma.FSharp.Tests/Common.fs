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

module CustomDatatypes =
    [<Struct>]
    type WrappedInt =
        val mutable InnerValue: int
        new(x) = { InnerValue = x }

        static member (+) (x: WrappedInt, y: WrappedInt) =
            WrappedInt(x.InnerValue + y.InnerValue)

        static member (-) (x: WrappedInt, y: WrappedInt) =
            WrappedInt(x.InnerValue - y.InnerValue)

    [<Struct>]
    type StructOfIntFloat =
        val mutable X: int
        val mutable Y: float

    [<Struct>]
    type StructOfBoolBool =
        val mutable X: bool
        val mutable Y: bool

    [<Struct>]
    type GenericStruct<'a, 'b> =
        val mutable X: 'a
        val mutable Y: 'b
        new(x, y) = { X = x; Y = y }

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
        let kernel = context.CreateClKernel command
        kernel.Code

    let openclTranslate (expr: Expr) =
        let translator = FSQuotationToOpenCLTranslator()
        let (ast, methods) = translator.Translate(expr)
        printfn "%A" methods
        print ast

    let openclTransformQuotation (expr: Expr) =
        QuotationTransformers.Transformer.transformQuotation expr []

module A =
    open CustomDatatypes

    let inline f () =
        <@ fun (gid: int) (buffer: clarray<struct('a * 'b)>) ->
            let struct(a, b) = buffer.[gid]
            printfn "%i" a
            printf "%i" b
            buffer.[gid] <- struct(a, b) @>

    let inline g () = <@ fun (gid: int) (buffer: clarray<GenericStruct<'a, 'b>>) -> let x = buffer.[gid].X in let y = buffer.[gid].Y in buffer.[gid] <- GenericStruct(x, y) @>

    let s = [
        <@@ fun (range: Range1D) (buffer: ClArray<struct(int * int)>) -> (%f ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<struct(int * float)>) -> (%f ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<struct(bool * bool)>) -> (%f ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<struct((int * int) * (int * int))>) -> (%f ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<struct((int * float) * (bool * bool))>) -> (%f ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<struct(StructOfIntFloat * StructOfBoolBool)>) -> (%f ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<struct(GenericStruct<int, float> * GenericStruct<bool, bool>)>) -> (%f ()) range.GlobalID0 buffer @@>

        // <@@ fun (range: Range1D) (buffer: ClArray<GenericStruct<int, bool>>) -> (%g ()) range.GlobalID0 buffer @@>
        // <@@ fun (range: Range1D) (buffer: ClArray<GenericStruct<(int * float), (bool * bool)>>) -> (%g ()) range.GlobalID0 buffer @@>

        // <@@ fun (range: Range1D) (buffer: ClArray<struct(int * int * int * int * int * int * int * int * int * int)>) ->
        //     let gid = range.GlobalID0
        //     let struct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = buffer.[gid] in buffer.[gid] <- struct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        // @@>

        // <@@ fun (range: Range1D) (buffer: ClArray<struct((int * int) * (int * int))>) ->
        //     let gid = range.GlobalID0
        //     let struct((a, b), (c, d)) = buffer.[gid]
        //     buffer.[gid] <- struct((a, b), (c, d))
        // @@>

        // больше тестов на алигмент
    ]
