// Copyright (c) 2012, 2013 Semyon Grigorev <rsdpisuy@gmail.com>
// All rights reserved.
//
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.

module Brahma.FSharp.OpenCL.Core

open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL
open OpenCL.Net
open System

type CLCodeGenerator() =
    static member KernelName = "brahmaKernel"

    static member GenerateKernel(lambda: Expr, provider: ComputeProvider, kernel: ICLKernel, translatorOptions) =
        let codeGenerator = Translator.FSQuotationToOpenCLTranslator()
        let (ast, newLambda) = codeGenerator.Translate(lambda, translatorOptions)
        let code = Printer.AST.print ast

        kernel.Provider <- provider
        kernel.Source <- kernel.Source.Append code
        kernel.SetClosures [||]
        kernel.SetParameters []

        newLambda

type ComputeProvider with
    member private this.CompileQuery<'T when 'T :> ICLKernel>
        (
            lambda: Expr,
            translatorOptions,
            additionalSources: string list
        ) =

        let devices = Array.ofSeq this.Devices

        let getProgramProcessingError errCode program =
            devices
            |> Array.map
                (fun device ->
                    Cl.GetProgramBuildInfo(program, device, ProgramBuildInfo.Log)
                    |> fst
                    |> string
                )
            |> String.concat "\n"
            |> fun s -> failwithf "%s\nError code: %A" s errCode

        let kernel = System.Activator.CreateInstance<'T>()
        let r = CLCodeGenerator.GenerateKernel(lambda, this, kernel, translatorOptions)
        let mainSrc = (kernel :> ICLKernel).Source.ToString()

        let (program, error) =
            let sources = additionalSources @ [ mainSrc ] |> List.toArray
            Cl.CreateProgramWithSource(this.Context, uint32 (sources.Length), sources, null)

        if error <> ErrorCode.Success then
            getProgramProcessingError error program

        let error =
            Cl.BuildProgram(program, devices.Length |> uint32, devices, this.CompileOptionsStr, null, IntPtr.Zero)

        if error <> ErrorCode.Success then
            getProgramProcessingError error program

        let (clKernel, error) =
            Cl.CreateKernel(program, CLCodeGenerator.KernelName)

        if error <> ErrorCode.Success then
            failwithf "OpenCL kernel creation problem. Error code: %A" error

        (kernel :> ICLKernel).ClKernel <- clKernel

        kernel

    member this.Compile
        (
            query: Expr<'TRange -> 'a>,
            ?options: CompileOptions,
            ?translatorOptions,
            ?outCode: string ref,
            ?kernelName: string,
            ?additionalSources: string list
        ) =

        let options = defaultArg options ComputeProvider.DefaultOptions_p
        let tOptions = defaultArg translatorOptions []
        let additionalSources = defaultArg additionalSources []

        this.SetCompileOptions options

        let kernel =
            this.CompileQuery<Kernel<'TRange>>(query, tOptions, additionalSources)

        let rng = ref Unchecked.defaultof<'TRange>
        let args = ref [||]
        let run = ref Unchecked.defaultof<Commands.Run<'TRange>>

        let getStarterFuncton qExpr =
            // TODO to flat Lambdas
            let rec go expr vars =
                match expr with
                | Patterns.Lambda (v, body) -> Expr.Lambda(v, go body (v :: vars))
                | _ ->
                    let c =
                        Expr.NewArray(
                            typeof<obj>,
                            vars
                            |> List.rev
                            |> List.map (fun v -> Expr.Coerce(Expr.Var(v), typeof<obj>))
                        )

                    <@@
                        let x = %%c |> List.ofArray
                        rng := (box x.Head) :?> 'TRange
                        args := x.Tail |> Array.ofList

                        let brahmsRunCls =
                            new Brahma.OpenCL.Commands.Run<_>(kernel, !rng)

                        !args
                        |> Array.iteri (fun i x -> brahmsRunCls.SetupArgument(1, i, x))

                        run := kernel.Run(!rng, !args)
                    @@>

            <@ %%(go qExpr []): 'TRange -> 'a @>.Compile()

        if outCode.IsSome then
            (outCode.Value) := (kernel :> ICLKernel).Source.ToString()

        kernel, getStarterFuncton query, (fun () -> !run)
