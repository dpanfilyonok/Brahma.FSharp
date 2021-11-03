namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Translator
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

type ClKernel<'TRange, 'a when 'TRange :> INDRangeDimension>
    (
        clContext: IContext,
        srcLambda: Expr<'TRange ->'a>,
        ?kernelName
    ) =

    let kernelName = defaultArg kernelName "brahmaKernel"

    let (clCode, newLambda) =
        let (ast, newLambda) = clContext.Translator.Translate(srcLambda)
        let code = Printer.AST.print ast
        code, newLambda

    let compileQuery additionalSources =
        let (program, error) =
            let sources = additionalSources @ [clCode] |> List.toArray
            Cl.CreateProgramWithSource(clContext.Context, uint32 sources.Length, sources, null)

        if error <> ErrorCode.Success then
            failwithf "Program creation failed: %A" error

        let options = " -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations "
        let error = Cl.BuildProgram(program, 1u, [| clContext.Device |], options, null, IntPtr.Zero)

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

    let mutexBuffers = ResizeArray<IBuffer<Mutex>>()

    interface IKernel<'TRange, 'a> with
        member this.ArgumentsSetter =
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
                                | var when var.Type.Name.ToLower().StartsWith ClArray_ ->
                                    Expr.PropertyGet(
                                        Expr.Var var,
                                        typeof<IBuffer<_>>.GetProperty("Length")
                                    )

                                | var when var.Type.Name.ToLower().StartsWith ClCell_ ->
                                    Expr.Value 1

                                | _ ->
                                    failwithf "Something went wrong with type of atomic global var. \
                                    Expected var of type '%s' or '%s', but given %s" ClArray_ ClCell_ var.Type.Name
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
                            printfn "%A" (%%mutexLengths : int[])
                            let mutexArgs =
                                (%%mutexLengths : int[])
                                |> List.ofArray
                                |> List.map (fun n ->
                                    let mutexBuffer = new ClBuffer<Mutex>(clContext, Size n)
                                    mutexBuffers.Add mutexBuffer
                                    box mutexBuffer
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

        member this.Kernel = kernel
        member this.Range = !range :> INDRangeDimension
        member this.Code = clCode
        member this.ReleaseBuffers() =
            mutexBuffers
            |> Seq.iter (Msg.CreateFreeMsg >> clContext.CommandQueue.Post)

            mutexBuffers.Clear()

    member this.ArgumentsSetter = (this :> IKernel<_,_>).ArgumentsSetter
    member this.Kernel = (this :> IKernel<_,_>).Kernel
    member this.Range = (this :> IKernel<_,_>).Range
    member this.Code = (this :> IKernel<_,_>).Code

    /// Освобождает только временные промежуточные утилитарные буферы (например, буфер для мьютексов)
    member this.ReleaseBuffers() = (this :> IKernel<_,_>).ReleaseBuffers()
