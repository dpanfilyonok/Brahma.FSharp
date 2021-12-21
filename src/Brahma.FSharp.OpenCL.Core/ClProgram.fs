namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Printer
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

type ClProgram<'TRange, 'a when 'TRange :> INDRange>
    (
        clContext: IContext,
        srcLambda: Expr<'TRange ->'a>
    ) =

    let (clCode, newLambda) =
        let (ast, newLambda) = clContext.Translator.Translate(srcLambda)
        let code = AST.print ast
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

    let createKernel program kernelName =
        let (clKernel, error) = Cl.CreateKernel(program, kernelName)
        if error <> ErrorCode.Success then
            failwithf "OpenCL kernel creation problem. Error: %A" error
        clKernel

    member this.Program = program

    member this.Code = clCode

    member this.NewKernel(?kernelName) =
        let kernelName = defaultArg kernelName "brahmaKernel"
        let kernel = createKernel program kernelName

        let toIMem arg =
            match box arg with
            | :? IClMem as buf -> buf.Size, buf.Data
            | :? int as i -> IntPtr(Marshal.SizeOf i), box i
            | other -> failwithf "Unexpected argument: %A" other

        let setupArgument index (arg: obj) =
            let (argSize, argVal) = toIMem arg
            let error = Cl.SetKernelArg(kernel, uint32 index, argSize, argVal)
            if error <> ErrorCode.Success then
                raise (CLException error)

        let args = ref [||]
        let range = ref Unchecked.defaultof<'TRange>
        let mutexBuffers = ResizeArray<IBuffer<Mutex>>()

        let argumentsSetterFunc =
            match newLambda with
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

            | _ -> failwithf "Invalid kernel expression. Must be lambda, but given\n%O" newLambda

            |> fun kernelPrepare ->
                <@ %%kernelPrepare : 'TRange -> 'a @>.Compile()

        { new IKernel<'TRange, 'a> with
            member _.Kernel = kernel
            member _.NDRange = range.Value :> INDRange
            member _.KernelFunc = argumentsSetterFunc
            member _.ReleaseInternalBuffers() =
                mutexBuffers
                |> Seq.iter (Msg.CreateFreeMsg >> clContext.CommandQueue.Post)

                mutexBuffers.Clear()
        }

