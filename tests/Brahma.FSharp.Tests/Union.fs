module Brahma.FSharp.Tests.Union

open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Printer

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

[<Tests>]
let unionTestCases =
    let defaultMsg = "Should be equal"

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
        testList "Translate union"
            [
                ptestCase "Test 1" <| fun _ ->
                    let unions = Type.translateDiscriminatedUnionDecls [typeof<TranslateTest>]
                    let code = AST.Print <| AST [StructDecl unions.[0]]
                    printfn "%s" code
            ]

    testList "Tests for translator"
        [
            collectUnionTests
            translateUnionTests
        ]
    |> (fun x -> Expecto.Sequenced (Expecto.SequenceMethod.Synchronous, x))
