module Brahma.FSharp.OpenCL.OpenCLTranslator.Main

open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.OpenCLTranslator.Lexer
open Brahma.FSharp.OpenCL.OpenCLTranslator.Parser
open Brahma.FSharp.OpenCL.OpenCLTranslator.Validator
open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser

module OpenCLParser = Brahma.FSharp.OpenCL.OpenCLTranslator.Parser

let parseCLCode code =
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString code
    let allTokens =
        seq
            {
                while not lexbuf.IsPastEndOfStream do yield token lexbuf
            }

    let translateArgs = {
        tokenToRange = fun x -> 0UL, 0UL
        zeroPosition = 0UL
        clearAST = false //try to make a tree (default is false assuming OK)
        filterEpsilons = true // filtering eps-cycles
    }

    let tree =
        match OpenCLParser.buildAst allTokens with
        | Success (sppf, t, d) ->
            let translationResult = (OpenCLParser.translate translateArgs sppf d) :> obj
            match translationResult with
            | :? List<List<FunDecl<Lang>>> as res -> res
            | _ -> failwithf "Error: translator returned an invalud type: %A" (translationResult.GetType())
        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg

    let res = tree.[0]
    List.iter (fun x -> checkFunDecl x) res
    res
