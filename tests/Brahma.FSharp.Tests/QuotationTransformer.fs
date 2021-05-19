module Brahma.FSharp.Tests.QuotationTransformer

open Brahma.FSharp.Tests

open Expecto
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LambdaLifting.LambdaLifting
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.LetVarAbstracter
open Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations

[<Tests>]
let quotationTransformerTests =
    let eqMsg = "Values should be equal"

    let rec renameUnitVar (expr: Expr) =
        let replaceUnitVar (var: Var) =
            if var.Type = typeof<unit>
                then Var("unitVar", var.Type, var.IsMutable)
                else var

        match expr with
        | ExprShape.ShapeVar var ->
            Expr.Var(replaceUnitVar var)
        | ExprShape.ShapeLambda (var, body) ->
            Expr.Lambda(replaceUnitVar var, renameUnitVar body)
        | ExprShape.ShapeCombination (shapeComboObj, exprList) ->
            ExprShape.RebuildShapeCombination(shapeComboObj, List.map renameUnitVar exprList)

    let assertActual (actual: Expr) (expected: Expr) =
        let actual' = renameUnitVar actual
        let expected' = renameUnitVar expected
        Expect.equal <| actual'.ToString() <| expected'.ToString() <| eqMsg

    let lambdaLiftingTests =
        let genParameterLiftTest testCase name expr expected =
            testCase name <| fun _ ->
                let actual = parameterLiftExpr expr
                assertActual actual expected

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

    let varDefsToLambdaTest =
        let genVarDefToLambdaTest testCase name expr expected =
            testCase name <| fun _ ->
                let actual = varDefsToLambda expr
                assertActual actual expected

        testList "Var defs to lambda test" [
            genVarDefToLambdaTest testCase "Test 1"
                <@
                    let x =
                        let mutable y = 0
                        for i in 1..10 do
                            y <- y + i
                        y
                    x
                @>
                <@
                    let x =
                        let xUnitFunc () =
                            let mutable y = 0
                            for i in 1..10 do
                                y <- y + i
                            y
                        xUnitFunc ()
                    x
                @>

            genVarDefToLambdaTest testCase "Test 2: we need to go deeper"
                <@
                    let x =
                        let mutable y =
                            if true
                            then
                                let z = 10
                                z + 1
                            else
                                let z = 20
                                z + 2

                        for i in 1..10 do
                            let z = if false then 10 else 20
                            y <- y + i + z
                        y
                    x
                @>
                <@
                    let x =
                        let xUnitFunc () =
                            let mutable y =
                                let yUnitFunc () =
                                    if true
                                    then
                                        let z = 10
                                        z + 1
                                    else
                                        let z = 20
                                        z + 2
                                yUnitFunc ()

                            for i in 1..10 do
                                let z =
                                    let zUnitFunc () =
                                        if false then 10 else 20
                                    zUnitFunc ()
                                y <- y + i + z
                            y
                        xUnitFunc ()
                    x
                @>
        ]

    testList "Quotation transformer tests"
        [
            lambdaLiftingTests
            varDefsToLambdaTest
        ]
