namespace Brahma.FSharp.OpenCL.Core

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Translator
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

type ClKernel<'TRange, 'a when 'TRange :> INDRange>
    (
        program: Program,
        newLambda: Expr,
        context: RuntimeContext,
        ?kernelName
    ) =

    let kernelName = defaultArg kernelName "brahmaKernel"

    let kernel =
        let (clKernel, error) = Cl.CreateKernel(program, kernelName)
        if error <> ErrorCode.Success then
            failwithf $"OpenCL kernel creation problem. Error: %A{error}"
        clKernel

    let toIMem arg =
        match box arg with
        | :? IClMem as buf -> buf.Size, buf.Data
        | :? int as i -> IntPtr(Marshal.SizeOf i), box i
        | other -> failwithf $"Unexpected argument: %A{other}"

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
                            failwithf $"Something went wrong with type of atomic global var. \
                            Expected var of type '%s{ClArray_}' or '%s{ClCell_}', but given %s{var.Type.Name}"
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
                            let mutexBuffer = new ClBuffer<Mutex>(context.ClContext, context.Translator, Size n)
                            mutexBuffers.Add mutexBuffer
                            box mutexBuffer
                        )

                    let x = %%regularArgs |> List.ofArray
                    range.Value <- unbox<'TRange> x.Head
                    args.Value <- x.Tail @ mutexArgs |> Array.ofList

                    args.Value
                    |> Array.iteri setupArgument
                @@>
            )

        | _ -> failwithf $"Invalid kernel expression. Must be lambda, but given\n{newLambda}"

        |> fun kernelPrepare ->
            <@ %%kernelPrepare : 'TRange -> 'a @>.Compile()

    interface IKernel<'TRange, 'a> with
        member this.Kernel = kernel
        member this.NDRange = range.Value :> INDRange
        member this.KernelFunc = argumentsSetterFunc
        member this.ReleaseInternalBuffers() =
            mutexBuffers
            |> Seq.iter (Msg.CreateFreeMsg >> context.CommandQueue.Post)

            mutexBuffers.Clear()

    // TODO rewrite
    member this.Kernel = kernel
    member this.NDRange = range.Value :> INDRange
    member this.KernelFunc = argumentsSetterFunc
    member this.ReleaseInternalBuffers() =
        mutexBuffers
        |> Seq.iter (Msg.CreateFreeMsg >> context.CommandQueue.Post)

        mutexBuffers.Clear()
