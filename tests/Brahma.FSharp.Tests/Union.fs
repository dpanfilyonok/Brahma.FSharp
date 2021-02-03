module Brahma.FSharp.Tests.Union

open Expecto
open Brahma.FSharp.OpenCL.Translator

type SimpleUnion =
    | SimpleOne
    | SimpleTwo of int

type OuterUnion =
    | Outer of int
    | Inner of SimpleUnion


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
    collectUnionTests
