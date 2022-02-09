namespace Brahma.FSharp.OpenCL

open Brahma.FSharp.OpenCL
open OpenCL.Net
open Brahma.FSharp.OpenCL.Translator

type IContext =
    abstract Device : Device
    abstract Context : Context

type IRuntimeContext =
    abstract ClContext : IContext
    abstract Translator : FSQuotationToOpenCLTranslator with get
    abstract CommandQueue : MailboxProcessor<Msg>
