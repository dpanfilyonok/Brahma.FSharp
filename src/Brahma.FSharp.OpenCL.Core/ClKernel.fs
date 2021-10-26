namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Translator
open System
open System.Runtime.InteropServices

type ClProgram<'TRange, 'a when 'TRange :> INDRangeDimension> (
        clContext: IContext,
        srcLambda: Expr<'TRange ->'a>
    ) = 

    let (clCode, newLambda) =
        let (ast, newLambda) = clContext.Translator.Translate(srcLambda)
        let code = Printer.AST.print ast
        code, newLambda

    let program =
        let (program, error) =
            let sources = [|clCode|]
            Cl.CreateProgramWithSource(clContext.Context, uint32 sources.Length, sources, null)

        if error <> ErrorCode.Success then
            failwithf "Program creation failed: %A" error

        let options = " -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations "
        let error = Cl.BuildProgram(program, 1u, [| clContext.Device |], options, null, IntPtr.Zero)

        if error <> ErrorCode.Success then
            failwithf "Program compilation failed: %A" error

        program

    member this.GetNewKernel (?kernelName) = new ClKernel<'TRange, 'a>(this, ?kernelName=kernelName) 
    member this.Program = program
    member this.Code = clCode
    member this.SetArguments range setupArgument =
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

                // TODO fix atomics
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

and ClKernel<'TRange, 'a when 'TRange :> INDRangeDimension>
    (
        clProgram: ClProgram<'TRange, 'a>,
        //setArgsLambda: 'TRange ->'a,
        ?kernelName:string
    ) =

    let kernelName = defaultArg kernelName "brahmaKernel"

    let createKernel program =
        let (clKernel, error) = Cl.CreateKernel(program, kernelName)
        if error <> ErrorCode.Success then
            failwithf "OpenCL kernel creation problem. Error: %A" error
        clKernel

    let kernel =
        let clKernel = createKernel clProgram.Program
        clKernel

    let toIMem a =
        // TODO extend types for private args (now only int supported)
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

    interface IKernel<'TRange, 'a> with
        member this.SetArguments =
            clProgram.SetArguments range setupArgument

        member this.Kernel = kernel
        member this.Range = !range :> INDRangeDimension
        member this.ReleaseAllBuffers() = () //usedBuffers := [||]
        member this.Code = clProgram.Code

    member this.SetArguments = (this :> IKernel<_,_>).SetArguments
    member this.Kernel = (this :> IKernel<_,_>).Kernel
    member this.Range = (this :> IKernel<_,_>).Range
    member this.Code = (this :> IKernel<_,_>).Code
    member this.ReleaseAllBuffers() = (this :> IKernel<_,_>).ReleaseAllBuffers

