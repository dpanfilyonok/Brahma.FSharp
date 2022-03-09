namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL.Translator

type CompilationContext =
    {
        ClContext: ClContext
        Translator: FSQuotationToOpenCLTranslator
        CompilerOptions: string option
    }

    static member Create(clContext, translator, options) =
        {
            ClContext = clContext
            Translator = translator
            CompilerOptions = options
        }
