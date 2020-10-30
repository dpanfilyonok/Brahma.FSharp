namespace Brahma.FSharp.OpenCL.OpenCLTranslator.Tests

open NUnit.Framework
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.OpenCLTranslator.Main
open Brahma.FSharp.OpenCL.OpenCLTranslator.Tests.Helpers

[<TestFixture>]
type RealWorldTests() =
    [<Test>]
    member this.``Matrix * matrix``() =
        let code = "__kernel void myGEMM1(const int M, const int N, const int K,\n\
                                          const __global float* A,\n\
                                          const __global float* B,\n\
                                          __global float* C) {\n\
                        // Thread identifiers\n\
                        const int globalRow = get_global_id(0); // Row ID of C (0..M)\n\
                        const int globalCol = get_global_id(1); // Col ID of C (0..N)\n\
                        // Compute a single element (loop over K)\n\\n\
                        float acc = 0.0f;\n\
                        for (int k=0; k<K; k++) {\n\
                            acc += A[k*M + globalRow] * B[globalCol*K + k];\n\
                        }\n\\n\
                        // Store the result\n\
                        C[globalCol*M + globalRow] = acc;\n\
                    }"
        let f = func
                    (DeclSpecifierPack<Lang>(
                        funQual=Kernel,
                        typeSpec=PrimitiveType<Lang>(Void)
                    ))
                    "myGEMM1"
                    [
                        arg
                            (DeclSpecifierPack<Lang>(
                                typeSpec=PrimitiveType<Lang>(Int), typeQuals=[TypeQualifier.Const]))
                            "M";
                        arg
                            (DeclSpecifierPack<Lang>(
                                typeSpec=PrimitiveType<Lang>(Int), typeQuals=[TypeQualifier.Const]
                            ))
                            "N";
                        arg
                            (DeclSpecifierPack<Lang>(
                                typeSpec=PrimitiveType<Lang>(Int), typeQuals=[TypeQualifier.Const]
                            ))
                            "K";
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Float), []),
                                typeQuals=[TypeQualifier.Const]
                            ))
                            "A";
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Float), []),
                                typeQuals=[TypeQualifier.Const]
                            ))
                            "B";
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Float), [])
                            ))
                            "C"
                    ]
        testSuccess code [f]

    [<Test>]
    member this.``Matrix * vector``() =
        let code = "__kernel void matvec(__global const float *A,\n\
                                         __global const float *x,\n\
                                         uint ncols,\n\
                                         __global float *y)\n\
                   {\n\
                       size_t i = get_global_id(0);\n\
                       __global float const *a = &A[i*ncols];\n\
                       float sum = 0.f;\n\
                       for (size_t j = 0; j < ncols; j++) {\n\
                           sum += a[j] * x[j];\n\
                       }\n\
                       y[i] = sum;\n\
                   }"
        let f = func
                    (DeclSpecifierPack<Lang>(
                        funQual=Kernel,
                        typeSpec=PrimitiveType<Lang>(Void)
                    ))
                    "matvec"
                    [
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Float), []),
                                typeQuals=[TypeQualifier.Const]
                            ))
                            "A";
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Float), []),
                                typeQuals=[TypeQualifier.Const]
                            ))
                            "x";
                        arg
                            (tdecl <| PrimitiveType<Lang>(UInt))
                            "ncols";
                        arg
                            (DeclSpecifierPack<Lang>(
                                addrSpaceQual=Global,
                                typeSpec=RefType<Lang>(PrimitiveType<Lang>(Float), [])
                            ))
                            "y"
                    ]
        testSuccess code [f]
