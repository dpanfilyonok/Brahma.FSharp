module Brahma.FSharp.OpenCL.Translator.TypeReflection

let hasAttribute<'attr> (tp: System.Type) =
    tp.GetCustomAttributes(false) |>
    Seq.tryFind (fun attr -> attr.GetType() = typeof<'attr>) |>
    Option.isSome

let isStruct = hasAttribute<StructAttribute>
