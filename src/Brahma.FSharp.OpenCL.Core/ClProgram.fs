namespace Brahma.FSharp

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Printer
open System

type ClProgram<'TRange, 'a when 'TRange :> INDRange>
    (
        ctx: ClContext,
        srcLambda: Expr<'TRange ->'a>
    ) =

    let compilerOptions = defaultArg ctx.CompilerOptions " -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations "

    let (clCode, newLambda) =
        let (ast, newLambda) = ctx.Translator.Translate(srcLambda)
        let code = AST.print ast
        code, newLambda

    let program =
        let (program, error) =
            let sources = [|clCode|]
            Cl.CreateProgramWithSource(ctx.Context, uint32 sources.Length, sources, null)

        if error <> ErrorCode.Success then
            failwithf $"Program creation failed: %A{error}"

        let error = Cl.BuildProgram(program, 1u, [| ctx.ClDevice.Device |], compilerOptions, null, IntPtr.Zero)

        if error <> ErrorCode.Success then
            let errorCode = ref ErrorCode.Success
            let buildInfo = Cl.GetProgramBuildInfo(program, ctx.ClDevice.Device, ProgramBuildInfo.Log, errorCode)
            failwithf $"Program compilation failed: %A{error} \n   BUILD LOG:\n %A{buildInfo} \n"

        program

    member this.Program = program

    member this.Code = clCode

    member this.Lambda = newLambda

    member this.ClContext = ctx
