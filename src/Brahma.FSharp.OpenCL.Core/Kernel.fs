namespace Brahma.FSharp.OpenCL

open OpenCL.Net

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator

type GpuKernel<'TRange, 'a, 't when 'TRange :> INDRangeDimension>(device, context, srcLambda: Expr<'TRange ->'a>) =

    let kernelName = "brahmaKernel"

    let clCode =
        let translatorOptions = []
        let codeGenerator = new Translator.FSQuotationToOpenCLTranslator()
        let ast, newLambda = codeGenerator.Translate srcLambda translatorOptions
        let code = Printer.AST.Print ast
        code

    let compileQuery translatorOptions additionalSources =

        let program, error =
            let sources = additionalSources @ [clCode] |> List.toArray
            Cl.CreateProgramWithSource(context, uint32 (sources.Length), sources, null)

        if error <> ErrorCode.Success
        then failwithf "Program creation failed: %A" error

        let options =  
            " -cl-fast-relaxed-math -cl-mad-enable "
        let error = Cl.BuildProgram(program, 1u, [|device|], options, null, System.IntPtr.Zero)

        if error <> ErrorCode.Success
        then failwithf "Program compilation failed: %A" error

        program

    let createKernel program =
        let clKernel,error = Cl.CreateKernel(program, kernelName)
        if error <> ErrorCode.Success
        then failwithf "OpenCL kernel creation problem. Error: %A" error
        clKernel

    let additionalSources = []

    let kernel =
        let program = compileQuery [] additionalSources
        let clKernel = createKernel program
        clKernel

    let toIMem a =
        match box a with
        | :? IClMem as buf ->
            buf.Size, buf.Data
        | :? int as i -> System.IntPtr(System.Runtime.InteropServices.Marshal.SizeOf(i)),
                         box i
        | x -> failwithf "Unexpected argument: %A" x

    let setupArgument index arg =
        let argSize, argVal = toIMem arg
        let error =
                Cl.SetKernelArg(kernel, (uint32 index)
                , argSize
                , argVal)
        if error <> ErrorCode.Success
        then raise (new Brahma.OpenCL.CLException(error))

    let range = ref Unchecked.defaultof<'TRange>

    let usedBuffers = ref [||]

    member this.SetArguments =
        let args = ref [||]
        let getStarterFunction qExpr =
            let rec go expr vars =
                match expr with
                | Patterns.Lambda (v, body) ->
                    let v =
                        if v.Type.IsArray
                        then
                            let vName = v.Name
                            let vType = typedefof<Buffer<_>>.MakeGenericType(v.Type.GetElementType())
                            Var(vName, vType, false)
                        else v
                    Expr.Lambda(v, go body (v::vars))
                | e ->
                    let arr =
                        let allArgs =
                            let expr (v:Var) = Expr.Var(v)

                            Expr.NewArray(
                                    typeof<obj>,
                                    vars |> List.rev |> List.map (fun v -> Expr.Coerce ((expr v), typeof<obj>))
                                    )
                            (*Expr.NewArray(
                                    typeof<obj>,
                                    vars
                                    |> List.choose 
                                        (fun v ->
                                            if v.Type.Name.Contains "Buffer" 
                                            then Some (Expr.Coerce (Expr.Var(v), typeof<obj>))
                                            else None
                                        )
                                    )*)
                        <@@
                            let x = %%allArgs |> List.ofArray
                            range := (box x.Head) :?> 'TRange
                            args := x.Tail |> Array.ofList
                            //let b =  %%buffers
                            //usedBuffers := b 
                            !args |> Array.iteri (fun i x -> setupArgument i x)
                        @@>
                    arr
            let res =
                let e:(Expr<'TRange -> 't>) = Expr.Cast (go qExpr [])
                <@ %e @>.Compile()

            res
        getStarterFunction srcLambda

    member this.ClKernel = kernel
    member this.Range = !range :> INDRangeDimension
    member this.ReleaseAllBuffers () = usedBuffers := [||]
