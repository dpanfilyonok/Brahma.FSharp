namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Brahma.FSharp.OpenCL.Translator

type IContext =
    abstract Context : Context
    abstract Device : Device
    abstract Translator : FSQuotationToOpenCLTranslator with get
    abstract Provider : ComputeProvider

