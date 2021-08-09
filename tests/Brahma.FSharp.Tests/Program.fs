open Expecto

open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.Tests
open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Core.LanguagePrimitives
open Brahma.FSharp.OpenCL.WorkflowBuilder
open Brahma.FSharp.OpenCL.Extensions
open Brahma.OpenCL
open OpenCL.Net

[<Tests>]
let allTests =
    testList "All tests" [
        Translator.tests
        Full.tests
        Atomic.tests
        Workflow.tests
        QuotationTransformers.tests
        Union.tests
    ]

[<EntryPoint>]
let main argv =
    allTests
    |> runTestsWithCLIArgs [] argv
