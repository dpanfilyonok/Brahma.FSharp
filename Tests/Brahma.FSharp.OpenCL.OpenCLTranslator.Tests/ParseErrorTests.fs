namespace Brahma.FSharp.OpenCL.OpenCLTranslator.Tests

open NUnit.Framework
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.OpenCLTranslator.Main

[<TestFixture>]
type ParseErrorTests() =
    let testParseError code errMessage =
        try
            parseCLCode code |> ignore
            Assert.Fail("Should have caught an exception here!")
        with
        | e -> Assert.AreEqual(errMessage, e.Message)

    [<Test>]
    member this.``Error: missing return type``() =
        let code = "__kernel global static epicKernel() { return; }"
        testParseError code "Fatal: return type missing for function epicKernel"

    [<Test>]
    [<ExpectedException(typeof<System.Exception>)>]
    member this.``Error: type of parameter not specified``() =
        parseCLCode "void foo(int a, b);" |> ignore

    [<Test>]
    member this.``Error: non-void kernel``() =
        let code = "kernel double bar(int *a);"
        testParseError code "Fatal: return type of kernel function bar must be void"

    [<Test>]
    member this.``Error: kernel parameter``() =
        let code = "void foo(kernel const int a, global unsigned long b);"
        testParseError code "Fatal: parameter a of function foo can't be kernel"

    [<Test>]
    member this.``Error: mismatching braces 1``() =
        let code = "void foo(struct bar baz) { if (true) { { } }"
        testParseError code "Mismatching curly braces"

    [<Test>]
    member this.``Error: mismatching braces 2``() =
        let code = "void foo(struct bar baz) { if (true) { } } }"
        testParseError code "Unexpected input: }"
