module Brahma.FSharp.Tests.QuotationTransformer

open Expecto
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LambdaLifting.LambdaLifting

[<Tests>]
let quotationTransformerTests =
    let eqMsg = "Values should be equal"

    let lambdaLiftingTests =
        let genParameterLiftTest testCase name expr expected =
            testCase name <| fun _ ->
                let actual = parameterLiftExpr expr
                Expect.equal <| actual.ToString() <| expected.ToString() <| eqMsg

        testList "Parameter lifting test" [
            genParameterLiftTest testCase "Test 1"
                <@
                    let x = 1
                    let addToX y = x + y
                    addToX 2
                @>
                <@
                    let x = 1
                    let addToX x y = x + y
                    addToX x 2
                @>

            genParameterLiftTest testCase "Test 2"
                <@
                    let x = 1
                    let z = x

                    let addToX y =  /// freeVars: [x, z]
                        x + y + z
                    let f z1 =      /// freeVars: [], addToX freeVars: [x, z]
                        2 + addToX z1
                    f 3
                @>
                <@
                    let x = 1
                    let z = x

                    let addToX x z y =
                        x + y + z
                    let f x z z1 =
                        2 + addToX x z z1
                    f x z 3
                @>

            genParameterLiftTest testCase "Test 3"
                <@
                    let mainX = "global variable"
                    let mainY = "global variable"
                    let mainZ = "global variable"

                    let foo fooX =
                        let fooY = "local variable of foo"
                        let bar barX =
                            mainX + fooY + barX
                        bar fooX + mainY

                    foo mainZ
                @>
                <@
                    let mainX = "global variable"
                    let mainY = "global variable"
                    let mainZ = "global variable"

                    let foo mainX mainY fooX =
                        let fooY = "local variable of foo"
                        let bar fooY mainX barX =
                            mainX + fooY + barX
                        bar fooY mainX fooX + mainY

                    foo mainX mainY mainZ
                @>

            genParameterLiftTest testCase "Test 4"
                <@
                    let x0 = 0
                    let f x1 =
                        let g x2 =
                            let h x3 =
                                x3 + x0
                            h x2
                        g x1
                    f x0
                @>
                <@
                    let x0 = 0
                    let f x0 x1 =
                        let g x0 x2 =
                            let h x0 x3 =
                                x3 + x0
                            h x0 x2
                        g x0 x1
                    f x0 x0
                @>
        ]

    lambdaLiftingTests
