module Brahma.FSharp.OpenCL.OpenCLTranslator.Validator

open Brahma.FSharp.OpenCL.AST

let checkFunDecl (f:FunDecl<Lang>) =
    // return type must be present
    let t =
        match f.DeclSpecs.Type with
        | Some tp -> tp
        | None -> failwithf "Fatal: return type missing for function %s" f.Name

    // return type of a kernel function must be void
    if f.DeclSpecs.FunQual = Some Kernel
        && not ((t :? PrimitiveType<_>) && (t :?> PrimitiveType<_>).Type = Void)
    then failwithf "Fatal: return type of kernel function %s must be void" f.Name

    // STATIC storage class specifier is allowed only for non-kernel functions
    if f.DeclSpecs.FunQual = Some Kernel && f.DeclSpecs.StorageClassSpec = Some Static
    then failwithf "Fatal: unexpected \'static\' modifier for kernel function %s" f.Name

    // validate parameters
    for p in f.Args do
        // type must be present
        if p.DeclSpecs.Type.IsNone
        then failwithf "Fatal: return type missing for parameter %s of function %s" p.Name f.Name

        // parameters can't be __kernel
        if p.DeclSpecs.FunQual = Some Kernel
        then failwithf "Fatal: parameter %s of function %s can't be kernel" p.Name f.Name

        // no storage class specifiers (TYPEDEF, EXTERN, STATIC) are allowed for parameters
        if p.DeclSpecs.StorageClassSpec.IsSome
        then failwithf "Fatal: unexpected storage class specifier for parameter %s of function %s" p.Name f.Name
    ()
