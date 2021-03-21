module Brahma.FSharp.OpenCL.QuotationsTransformer.Common

open FSharp.Reflection
open FSharp.Quotations

/// Function for replacing printf call
let print (tpArgs: System.Type list) (value: string) (bindArgs: Expr list) =
    ()

let rec getFunctionArgTypes (funType: System.Type) =
    let (argType, retType) = FSharpType.GetFunctionElements(funType)
    match retType with
    | _ when FSharpType.IsFunction retType ->
        argType :: getFunctionArgTypes retType
    | _ ->  [argType]
