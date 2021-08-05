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
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL
open OpenCL.Net
open System

type CLCodeGenerator() =
    static member KernelName = "brahmaKernel"

    static member GenerateKernel(lambda: Expr, provider: ComputeProvider, kernel: ICLKernel, translatorOptions) =
        let codeGenerator = FSQuotationToOpenCLTranslator()
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
        let newLambda = CLCodeGenerator.GenerateKernel(lambda, this, kernel, translatorOptions)

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

        kernel, newLambda

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

        let (kernel, newLambda) =
            this.CompileQuery<Kernel<'TRange>>(query, tOptions, additionalSources)

        if outCode.IsSome then
            outCode.Value := (kernel :> ICLKernel).Source.ToString()

        let rng = ref Unchecked.defaultof<'TRange>
        let args = ref [||]
        let run = ref Unchecked.defaultof<Commands.Run<'TRange>>

        let getStarterFuncton qExpr =
            match qExpr with
            | DerivedPatterns.Lambdas (lambdaArgs, _) ->
                let flattenArgs = List.collect id lambdaArgs

                let firstMutexIdx =
                    flattenArgs
                    |> List.tryFindIndex (fun v -> v.Name.EndsWith "Mutex")
                    |> Option.defaultValue flattenArgs.Length

                let argsWithoutMutexes = flattenArgs.[0 .. firstMutexIdx - 1]

                /// For each atomic variable returns 0 if variable's type is not array,
                /// otherwise returns length of array
                let mutexLengths =
                    let atomicVars =
                        List.init<Var> (flattenArgs.Length - firstMutexIdx) <| fun i ->
                            let mutexVar = flattenArgs.[firstMutexIdx + i]
                            argsWithoutMutexes |> List.find (fun v -> mutexVar.Name.Contains v.Name)

                    Expr.NewArray(
                        typeof<int>,

                        atomicVars
                        |> List.map (fun x ->
                            match x with
                            | x when x.Type.IsArray ->
                                Expr.PropertyGet(
                                    Expr.Var x,
                                    typeof<int[]>.GetProperty("Length")
                                )
                            | _ -> failwithf "kekw"
                        )
                    )

                let c =
                    Expr.NewArray(
                        typeof<obj>,

                        argsWithoutMutexes
                        |> List.map (fun v -> Expr.Coerce(Expr.Var v, typeof<obj>))
                    )

                Expr.Lambdas(
                    argsWithoutMutexes
                    |> List.map List.singleton,

                    <@@
                        let mutexArgs =
                            (%%mutexLengths : int[])
                            |> List.ofArray
                            |> List.map (fun n ->
                                if n = 0 then box 0
                                else box <| Array.zeroCreate<int> n
                            )

                        let x = %%c |> List.ofArray
                        rng := unbox<'TRange> x.Head
                        args := x.Tail @ mutexArgs |> Array.ofList

                        let brahmaRunCl = Commands.Run<_>(kernel, !rng)

                        !args
                        |> Array.iteri (fun i x -> brahmaRunCl.SetupArgument(1, i, x))

                        run := kernel.Run(!rng, !args)
                    @@>
                )

            | _ -> failwithf "Invalid expression. Must be lambda, but given\n%O" qExpr

            |> fun kernelPrepare ->
                <@ %%kernelPrepare: 'TRange -> 'a @>.Compile()

        kernel, getStarterFuncton newLambda, (fun () -> !run)
