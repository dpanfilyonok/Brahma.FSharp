namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Translator
open System
open System.Runtime.InteropServices

type ClKernel<'TRange, 'a when 'TRange :> INDRangeDimension>
    (
        provider: ComputeProvider,
        srcLambda: Expr<'TRange ->'a>,
        ?kernelName
    ) =

    let kernelName = defaultArg kernelName "brahmaKernel"

    let (clCode, newLambda) =
        let (ast, newLambda) = provider.Translator.Translate(srcLambda)
        let code = Printer.AST.print ast
        code, newLambda

    let compileQuery additionalSources =
        let (program, error) =
            let sources = additionalSources @ [clCode] |> List.toArray
            Cl.CreateProgramWithSource(provider.ClContext, uint32 sources.Length, sources, null)

        if error <> ErrorCode.Success then
            failwithf "Program creation failed: %A" error

        let options = " -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations "
        let error = Cl.BuildProgram(program, 1u, [| provider.ClDevice |], options, null, IntPtr.Zero)

        if error <> ErrorCode.Success then
            failwithf "Program compilation failed: %A" error

        program

    let createKernel program =
        let (clKernel, error) = Cl.CreateKernel(program, kernelName)
        if error <> ErrorCode.Success then
            failwithf "OpenCL kernel creation problem. Error: %A" error
        clKernel

    let additionalSources = []

    let kernel =
        let program = compileQuery additionalSources
        let clKernel = createKernel program
        clKernel

    let toIMem a =
        match box a with
        | :? IClMem as buf -> buf.Size, buf.Data
        | :? int as i -> IntPtr(Marshal.SizeOf i), box i
        | other -> failwithf "Unexpected argument: %A" other

    let setupArgument index arg =
        let (argSize, argVal) = toIMem arg
        let error = Cl.SetKernelArg(kernel, uint32 index, argSize, argVal)
        if error <> ErrorCode.Success then
            raise (CLException error)

    let range = ref Unchecked.defaultof<'TRange>

    let usedBuffers = ref [||]

    interface IKernel<'TRange, 'a> with
        member this.SetArguments =
            let args = ref [||]
            let getStarterFunction qExpr =
                match qExpr with
                | DerivedPatterns.Lambdas (lambdaArgs, _) ->
                    let flattenArgs = List.collect id lambdaArgs

                    let firstMutexIdx =
                        flattenArgs
                        |> List.tryFindIndex (fun v -> v.Name.EndsWith "Mutex")
                        |> Option.defaultValue flattenArgs.Length

                    let argsWithoutMutexes = flattenArgs.[0 .. firstMutexIdx - 1]

                    /// For each atomic variable throws exception if variable's type is not array,
                    /// otherwise returns length of array
                    let mutexLengths =
                        let atomicVars =
                            List.init<Var> (flattenArgs.Length - firstMutexIdx) <| fun i ->
                                let mutexVar = flattenArgs.[firstMutexIdx + i]
                                argsWithoutMutexes |> List.find (fun v -> mutexVar.Name.Contains v.Name)

                        Expr.NewArray(
                            typeof<int>,

                            atomicVars
                            |> List.map (fun var ->
                                match var with
                                | var when var.Type.IsArray ->
                                    Expr.PropertyGet(
                                        Expr.Var var,
                                        typeof<int[]>.GetProperty("Length")
                                    )
                                | _ -> failwith "Non-array variables as global mutex parameters is not supported. Wrap it into array instead."
                            )
                        )

                    let regularArgs =
                        Expr.NewArray(
                            typeof<obj>,
                            argsWithoutMutexes |> List.map (fun v -> Expr.Coerce(Expr.Var v, typeof<obj>))
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

                            let x = %%regularArgs |> List.ofArray
                            range := unbox<'TRange> x.Head
                            args := x.Tail @ mutexArgs |> Array.ofList

                            !args
                            |> Array.iteri setupArgument
                        @@>
                    )

                | _ -> failwithf "Invalid kernel expression. Must be lambda, but given\n%O" qExpr

                |> fun kernelPrepare ->
                    <@ %%kernelPrepare: 'TRange -> 'a @>.Compile()

            getStarterFunction newLambda

        member this.ClKernel = kernel
        member this.Range = !range :> INDRangeDimension
        member this.ReleaseAllBuffers() = usedBuffers := [||]
        member this.ClCode = clCode

    member this.SetArguments = (this :> IKernel<_,_>).SetArguments
    member this.ClKernel = (this :> IKernel<_,_>).ClKernel
    member this.Range = (this :> IKernel<_,_>).Range
    member this.ReleaseAllBuffers() = (this :> IKernel<_,_>).ReleaseAllBuffers
    member this.ClCode = (this :> IKernel<_,_>).ClCode
