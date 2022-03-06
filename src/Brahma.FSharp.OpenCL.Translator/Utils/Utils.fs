namespace Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open System.Collections.Generic
open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Reflection
open System

module Utils =
    let getMethodInfoOfCall (expr: Expr) =
        match expr with
        | Patterns.Call (_, mInfo, _) -> mInfo
        | DerivedPatterns.Lambdas (args, Patterns.Call (_, mInfo, _)) -> mInfo
        | _ -> failwithf $"Expression is not kind of call, but {expr}"

    let makeGenericMethodCall (types: System.Type list) (expr: Expr) =
        (getMethodInfoOfCall expr)
            .GetGenericMethodDefinition()
            .MakeGenericMethod(Array.ofList types)
