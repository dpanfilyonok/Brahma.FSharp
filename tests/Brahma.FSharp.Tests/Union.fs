module Brahma.FSharp.Tests.Union

open Expecto
open Brahma.FSharp.Tests.Common
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Printer
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core

type SimpleUnion =
    | SimpleOne
    | SimpleTwo of int

type OuterUnion =
    | Outer of int
    | Inner of SimpleUnion

type TranslateTest =
    | A of int * float
    | B of double
    | C

type TranslateMatchTestUnion =
    | Case1
    | Case2 of int
    | Case3 of int * int

[<Tests>]
let unionTestCases =
    let defaultMsg = "Should be equal"
    let basePath = "UnionExpected"

    let collectUnionTests =
        let testGen testCase name expected command =
            testCase name <| fun _ ->
                let unions = TypeReflection.CollectDiscriminatedUnions command
                Expect.sequenceEqual unions expected defaultMsg

        testList "Collect union tests"
            [
                testGen testCase "Simple union" [|typeof<SimpleUnion>|]
                    <@
                        let x = SimpleOne
                        let y = SimpleTwo 2
                        ()
                    @>

                testGen testCase "Nested union 1" [|typeof<SimpleUnion>; typeof<OuterUnion>|]
                    <@
                        let x = Outer 5
                        ()
                    @>

                testGen testCase "Nested union 2" [|typeof<SimpleUnion>; typeof<OuterUnion>|]
                    <@
                        let x = Inner <| SimpleOne
                        ()
                    @>
            ]

    let translateUnionTests =
        let testGen testCase name (types: List<System.Type>) outFile expectedFile =
            testCase name <| fun _ ->
                let context = TargetContext()
                let unions = Type.translateDiscriminatedUnionDecls types context
                let ast = AST <| List.map (fun du -> du :> TopDef<_>) unions
                let code = AST.Print ast

                System.IO.File.WriteAllText(outFile, code)
                filesAreEqual outFile <| System.IO.Path.Combine(basePath, expectedFile)

        testList "Translate union"
            [
                testGen testCase "Test 1" [typeof<TranslateTest>] "Translation.Test1.gen" "Translation.Test1.cl"
            ]

    let compileTests =
        let provider = ComputeProvider.Create()
        platformMessage provider "union compile tests"

        let testGen testCase name outFile expectedFile command =
            testCase name <| fun _ ->
                let code = ref ""
                provider.Compile(command, _outCode=code) |> ignore
                System.IO.File.WriteAllText(outFile, !code)
                filesAreEqual outFile <| System.IO.Path.Combine(basePath, expectedFile)

        let newUnionTestList =
            testList "NewUnion" [
                testGen testCase "Test 1: TranslateTest.A" "Union.Compile.Test1.gen" "Union.Compile.Test1.cl"
                    <@
                        fun (range: _1D) ->
                            let x = A(5, 6.0)
                            let mutable y = 5
                            y <- 7
                    @>

                testGen testCase "Test 2: TranslateTest.B" "Union.Compile.Test2.gen" "Union.Compile.Test2.cl"
                    <@
                        fun (range: _1D) ->
                            let x = B(5.0)
                            let mutable y = 5
                            y <- 7
                    @>

                testGen testCase "Test 3: TranslateTest.C" "Union.Compile.Test3.gen" "Union.Compile.Test3.cl"
                    <@
                        fun (range: _1D) ->
                            let x = C
                            let mutable y = 5
                            y <- 7
                    @>

                testGen testCase "Test 4: OuterUnion.Outer" "Union.Compile.Test4.gen" "Union.Compile.Test4.cl"
                    <@
                        fun (range: _1D) ->
                            let x = Inner SimpleOne
                            let mutable y = 5
                            y <- 7
                    @>

                testGen testCase "Test 5: OuterUnion.Inner" "Union.Compile.Test5.gen" "Union.Compile.Test5.cl"
                    <@
                        fun (range: _1D) ->
                            let x = Inner (SimpleTwo 29)
                            let mutable y = 5
                            y <- 7
                    @>
            ]

        let testUnionCaseTestLists =
            testList "TestUnionCase" [
                testGen testCase "Test 1: simple pattern matching" "Union.Compile.Test6.gen" "Union.Compile.Test6.cl"
                    <@
                    fun (range: _1D) ->
                        let t = Case1
                        let mutable x = 5

                        match t with
                        | Case1 -> x <- 5
                        | Case2(_) -> x <- 6
                        | Case3(_) -> x <- 7
                    @>
            ]

        let unionPropertyGetTestLists =
            testList "UnionPropertyGet" [
                testGen testCase "Test 1: simple pattern matching bindings" "Union.Compile.Test7.gen" "Union.Compile.Test7.cl"
                    <@
                    fun (range: _1D) ->
                        let t = Case1
                        let mutable m = 5

                        match t with
                        | Case1 -> m <- 5
                        | Case2(x) -> m <- x
                        | Case3(y, z) -> m <- y + z
                @>

            ]


        testList "Union Compile tests"
            [
                newUnionTestList
                testUnionCaseTestLists
                unionPropertyGetTestLists
            ]

    testList "Tests for translator"
        [
            collectUnionTests
            translateUnionTests
            compileTests
        ]
    |> (fun x -> Expecto.Sequenced (Expecto.SequenceMethod.Synchronous, x))
