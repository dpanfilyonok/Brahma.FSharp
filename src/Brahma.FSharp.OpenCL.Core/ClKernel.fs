namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Translator
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

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
            let errorCode = ref ErrorCode.Success
            let buildInfo = Cl.GetProgramBuildInfo(program, clContext.Device, ProgramBuildInfo.Log, errorCode)
            failwithf "Program compilation failed: %A \n   BUILD LOG:\n %A \n" error (buildInfo)

        program

    let mutexBuffers = ResizeArray<IBuffer<Mutex>>()

    member this.GetNewKernel (?kernelName) = new ClKernel<'TRange, 'a>(this, ?kernelName=kernelName) 
    member this.Program = program
    member this.Code = clCode
    member this.ArgumentsSetter range setupArgument =
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
                                        typeof<IBuffer<_>>
                                            .GetGenericTypeDefinition()
                                            .MakeGenericType(var.Type.GenericTypeArguments.[0])
                                            .GetProperty("Length")
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

    member this.ReleaseBuffers() =
            mutexBuffers
            |> Seq.iter (Msg.CreateFreeMsg >> clContext.CommandQueue.Post)

            mutexBuffers.Clear()


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
        // NOTE SetKernelArg could take intptr
        // TODO try allocate unmanaged mem by hand
        let error = Cl.SetKernelArg(kernel, uint32 index, argSize, argVal)
        if error <> ErrorCode.Success then
            raise (CLException error)

    let range = ref Unchecked.defaultof<'TRange>

    interface IKernel<'TRange, 'a> with
        member this.ArgumentsSetter =
            clProgram.ArgumentsSetter range setupArgument
            
        member this.Kernel = kernel
        member this.Range = range.Value :> INDRangeDimension
        member this.Code = clProgram.Code
        member this.ReleaseBuffers() = clProgram.ReleaseBuffers()

    member this.ArgumentsSetter = (this :> IKernel<_,_>).ArgumentsSetter
    member this.Kernel = (this :> IKernel<_,_>).Kernel
    member this.Range = (this :> IKernel<_,_>).Range
    member this.Code = (this :> IKernel<_,_>).Code

    // TODO rename ?? ReleaseInternalBuffers
    /// Освобождает только временные промежуточные утилитарные буферы (например, буфер для мьютексов)
    member this.ReleaseBuffers() = (this :> IKernel<_,_>).ReleaseBuffers()
