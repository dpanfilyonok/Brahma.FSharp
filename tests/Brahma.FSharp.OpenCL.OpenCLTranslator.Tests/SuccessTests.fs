namespace Brahma.FSharp.OpenCL.OpenCLTranslator.Tests

open NUnit.Framework
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.OpenCLTranslator.Tests.Helpers

[<TestFixture>]
type SuccessTests() =
    [<Test>]
    member this.``Single parameterless kernel``() =
        let code = "__kernel void epicKernel() { return; }"
        let f = func
                    (DeclSpecifierPack<Lang>(funQual=Kernel, typeSpec=PrimitiveType<Lang>(Void)))
                    "epicKernel"
                    []
        testSuccess code [f]

    [<Test>]
    member this.``Function body with inner braces``() =
        let code = "__kernel void epicKernel() {\n\
                    int ncols = 6, sum = 0;\n\
                    for (size_t j = 0; j < ncols; j++) { sum += 1; }\n\
                    return; }"
        let f = func
                    (DeclSpecifierPack<Lang>(funQual=Kernel, typeSpec=PrimitiveType<Lang>(Void)))
                    "epicKernel"
                    []
        testSuccess code [f]

    [<Test>]
    member this.``Three functions in one file``() =
        let code = "__kernel void epicKernel1() { return; }\n\
                    __kernel void epicKernel2(int bar) { return; }\n\
                    float epicFun(double baz) { return 2.0; }\n"
        let f1 = func
                    (DeclSpecifierPack<Lang>(funQual=Kernel, typeSpec=PrimitiveType<Lang>(Void)))
                    "epicKernel1"
                    []
        let f2 = func
                    (DeclSpecifierPack<Lang>(funQual=Kernel, typeSpec=PrimitiveType<Lang>(Void)))
                    "epicKernel2"
                    [ arg (tdecl <| PrimitiveType<Lang>(Int)) "bar" ]
        let f3 = func
                    (tdecl <| PrimitiveType<Lang>(Float))
                    "epicFun"
                    [ arg (tdecl <| PrimitiveType<Lang>(Double)) "baz" ]
        testSuccess code [f1; f2; f3]

    [<Test>]
    member this.``Extern kernel with pointer parameters``() =
        let code = "extern kernel void foo(int *a, char* *b);"
        let f = func
                    (DeclSpecifierPack<Lang>(
                        funQual=Kernel,
                        storClassSpec=Extern,
                        typeSpec=PrimitiveType<Lang>(Void)
                    ))
                    "foo"
                    [
                        arg (tdecl <| RefType<Lang>(PrimitiveType<Lang>(Int), [])) "a";
                        arg (tdecl <| RefType<Lang>(RefType<Lang>(PrimitiveType<Lang>(Char), []), [])) "b"
                    ]
        testSuccess code [f]

    [<Test>]
    member this.``Distinct modifiers, signed/unsigned parameters``() =
        let code = "extern kernel void foo(global volatile restrict signed int *a,\
                    __constant volatile unsigned long *b);"
        let f = func
                    (DeclSpecifierPack<Lang>(
                        funQual=Kernel,
                        storClassSpec=Extern,
                        typeSpec=PrimitiveType<Lang>(Void)
                    ))
                    "foo"
                    [
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Int), []),
                                typeQuals=[ TypeQualifier.Restrict; TypeQualifier.Volatile ]
                            ))
                            "a";
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Constant,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(ULong), []),
                                typeQuals=[ TypeQualifier.Volatile ]
                            ))
                            "b"
                    ]
        testSuccess code [f]

    [<Test>]
    member this.``Sneaky pointers, unsigned``() =
        let code = "unsigned int foo(const int *ptr_to_constant, unsigned int *const constant_ptr)\n\
                    { return *ptr_to_constant; }"
        let f = func
                    (DeclSpecifierPack<Lang>(typeSpec=PrimitiveType<Lang>(UInt)))
                    "foo"
                    [
                        arg
                            (DeclSpecifierPack<Lang>(
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Int), []),
                                typeQuals=[ TypeQualifier.Const ]
                            ))
                            "ptr_to_constant";
                        arg
                            (DeclSpecifierPack<Lang>(
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(UInt), [ TypeQualifier.Const ]),
                                typeQuals=[]
                            ))
                            "constant_ptr"
                    ]
        testSuccess code [f]

    [<Test>]
    member this.``Arrays and pointers, returning pointer``() =
        let code = "unsigned char* foo(double a[], int* b[])\n{}"
        let f = func
                    (DeclSpecifierPack<Lang>(typeSpec=RefType<Lang>(PrimitiveType<Lang>(UChar), [])))
                    "foo"
                    [
                        arg (tdecl <| ArrayType<Lang>(PrimitiveType<Lang>(Double))) "a";
                        arg (tdecl <| ArrayType<Lang>(RefType<Lang>(PrimitiveType<Lang>(Int), []))) "b"
                    ]
        testSuccess code [f]

    [<Test>]
    member this.``Structs``() =
        let code = "__kernel void foo(struct epicStruct a, struct epicStruct b[],\
                    struct epicStruct *c, struct epicStruct* d[])\n{ return; }"
        let epicStructType = StructType<Lang>(Some <| Struct<Lang>("epicStruct", []))
        let f = func
                    (DeclSpecifierPack<Lang>(funQual=Kernel, typeSpec=PrimitiveType<Lang>(Void)))
                    "foo"
                    [
                        arg (tdecl <| epicStructType) "a";
                        arg (tdecl <| ArrayType<Lang>(epicStructType)) "b";
                        arg (tdecl <| RefType<Lang>(epicStructType, [])) "c";
                        arg (tdecl <| ArrayType<Lang>(RefType<Lang>(epicStructType, []))) "d"
                    ]
        testSuccess code [f]
