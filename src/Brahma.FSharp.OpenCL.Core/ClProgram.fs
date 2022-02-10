namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Printer
open System

type ClProgram<'TRange, 'a when 'TRange :> INDRange>
    (
        clContext: ClContext,
        translator: FSQuotationToOpenCLTranslator,
        srcLambda: Expr<'TRange ->'a>,
        ?compilerOptions: string
    ) =

    let compilerOptions = defaultArg compilerOptions " -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations "

    let (clCode, newLambda) =
        let (ast, newLambda) = translator.Translate(srcLambda)
        let code = AST.print ast
        code, newLambda

    let program =
        let (program, error) =
            let sources = [|clCode|]
            Cl.CreateProgramWithSource(clContext.Context, uint32 sources.Length, sources, null)

        if error <> ErrorCode.Success then
            failwithf $"Program creation failed: %A{error}"

        let error = Cl.BuildProgram(program, 1u, [| clContext.Device |], compilerOptions, null, IntPtr.Zero)

        if error <> ErrorCode.Success then
            let errorCode = ref ErrorCode.Success
            let buildInfo = Cl.GetProgramBuildInfo(program, clContext.Device, ProgramBuildInfo.Log, errorCode)
            failwithf $"Program compilation failed: %A{error} \n   BUILD LOG:\n %A{buildInfo} \n"

        program

    member this.Program = program

    member this.Code = clCode

    member this.Lambda = newLambda
