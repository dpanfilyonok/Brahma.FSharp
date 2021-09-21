namespace Brahma.FSharp

[<AutoOpen>]
module ClKernelLangExt =
    let inline private openclKeywordFail () =
        failwith "Seems that you try to use openCL kernel function as regular F# function!"

    let barrier () =
        openclKeywordFail ()
        ignore null

    let local<'a when 'a: struct> () =
        openclKeywordFail ()
        Unchecked.defaultof<'a>

    let localArray<'a> (size: int) =
        openclKeywordFail ()
        Unchecked.defaultof<array<'a>>

    let atomic (f: 'a -> 'b) =
        openclKeywordFail ()
        f

    let inline inc (p: 'a) = openclKeywordFail (); p + LanguagePrimitives.GenericOne<'a>
    let inline dec (p: 'a) = openclKeywordFail (); p - LanguagePrimitives.GenericOne<'a>

    // работает для всех типов
    let inline xchg (p: 'a) (value: 'a) = openclKeywordFail (); p
    let inline cmpxchg (p: 'a) (cmp: 'a) (value: 'a) = openclKeywordFail (); if p = cmp then value else p
