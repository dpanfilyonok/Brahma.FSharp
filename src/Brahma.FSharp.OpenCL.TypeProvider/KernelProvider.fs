module Brahma.FSharp.OpenCL.TypeProvider.KernelProvider

open Brahma.FSharp.OpenCL.TypeProvider.KernelReader
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection

[<TypeProvider>]
type KernelProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let nspace = "Brahma.FSharp.OpenCL.TypeProvider.Provided"
    let assembly = Assembly.GetExecutingAssembly()

    let kernelProvider = ProvidedTypeDefinition(assembly, nspace, "KernelProvider", Some(typeof<obj>))
    let parameters =
        [
            ProvidedStaticParameter("PathToFile", typeof<string>)
            ProvidedStaticParameter("TreatPointersAsArrays", typeof<bool>, parameterDefaultValue = false)
        ]
    let xmlHelpText =
        """
        <summary>Provider for typed representation of OpenCL C kernel function headers.</summary>
        <param name="PathToFile">Location of a file containing OpenCL C source code.</param>
        <param name="TreatPointersAsArrays">When set to true, the type provider will generate array types
        for pointers in function parameters, instead of reference types.</param>
        """

    do kernelProvider.AddXmlDoc xmlHelpText
    do kernelProvider.DefineStaticParameters(parameters, fun typeName args ->
        let filePath = args.[0] :?> string
        let treatPointersAsArrays = args.[1] :?> bool
        let retProvider = ProvidedTypeDefinition(assembly,
                                                 nspace,
                                                 typeName,
                                                 Some typeof<obj>,
                                                 HideObjectMethods = true)
        readKernels filePath treatPointersAsArrays
        |> List.iter retProvider.AddMember
        retProvider
    )

    do
        this.AddNamespace(nspace, [kernelProvider])

[<assembly:TypeProviderAssembly>]
do ()
