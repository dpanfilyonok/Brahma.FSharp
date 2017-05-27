module Brahma.FSharp.OpenCL.OpenCLTranslator.Tests.Helpers

open NUnit.Framework
open Brahma.FSharp.OpenCL.OpenCLTranslator.Main
open Brahma.FSharp.OpenCL.AST

let emptyBody = new StatementBlock<_>(new ResizeArray<_>())

/// DeclSpecifierPack with type only
let tdecl t = DeclSpecifierPack<Lang>(typeSpec=t)

let func specs name args = FunDecl<Lang>(specs, name, args, emptyBody)

let arg specs name = FunFormalArg<Lang>(specs, name)

let testSuccess code res =
        let actualRes = parseCLCode code
        let eq =
            List.fold
                (fun eq ((x, y):(FunDecl<_> * FunDecl<_>)) -> eq && x.Matches(y))
                true
                (List.zip res actualRes)
        Assert.That(eq)
