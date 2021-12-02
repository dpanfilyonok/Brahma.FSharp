namespace rec Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST

[<AbstractClass>]
type Method(var: Var, expr: Expr) =
    member this.FunVar = var
    member this.FunExpr = expr

    member internal this.AddReturn(subAST) =
        let rec adding (stmt: Statement<'lang>) =
            match stmt with
            | :? StatementBlock<'lang> as sb ->
                let listStaments = sb.Statements
                let lastStatement = listStaments.[listStaments.Count - 1]
                sb.Remove(listStaments.Count - 1)
                sb.Append(adding lastStatement)
                sb :> Statement<_>
            | :? Expression<'lang> as ex -> Return ex :> Statement<_>
            | :? IfThenElse<'lang> as ite ->
                let newThen =
                    adding ite.Then :?> StatementBlock<_>

                let newElse =
                    if Option.isNone ite.Else then
                        None
                    else
                        Some (adding ite.Else.Value :?> StatementBlock<_>)

                IfThenElse(ite.Condition, newThen, newElse) :> Statement<_>
            | _ -> failwithf "Unsupported statement to add Return: %A" stmt

        adding subAST

    abstract TranslateBody : Var list * Expr -> State<TargetContext, StatementBlock<Lang>>
    default this.TranslateBody(args, body) = translation {
        let! context = State.get

        context.Namer.LetIn()
        args |> List.iter (fun v -> context.Namer.AddVar v.Name)

        let! newBody = Body.translate body

        return
            match newBody with
            | :? StatementBlock<Lang> as sb -> sb
            | :? Statement<Lang> as s -> StatementBlock <| ResizeArray [s]
            | _ -> failwithf "Incorrect function body: %A" newBody
    }

    abstract TranslateArgs : Var list * string list * string list -> State<TargetContext, FunFormalArg<Lang> list>

    abstract BuildFunction : FunFormalArg<Lang> list * StatementBlock<Lang> -> State<TargetContext, ITopDef<Lang>>

    abstract GetPragmas : unit -> State<TargetContext, ITopDef<Lang> list>
    default this.GetPragmas() = translation {
        let! context = State.get

        let pragmas = ResizeArray()

        if context.Flags.enableAtomic then
            pragmas.Add(CLPragma CLGlobalInt32BaseAtomics :> ITopDef<_>)
            pragmas.Add(CLPragma CLLocalInt32BaseAtomics :> ITopDef<_>)

        if context.Flags.enableFP64 then
            pragmas.Add(CLPragma CLFP64)

        return List.ofSeq pragmas
    }

    abstract GetTopLevelVarDecls : unit -> State<TargetContext, ITopDef<Lang> list>
    default this.GetTopLevelVarDecls() = translation {
        let! context = State.get

        return
            context.TopLevelVarsDecls
            |> Seq.cast<_>
            |> List.ofSeq
    }

    abstract Translate : string list * string list -> State<TargetContext, ITopDef<Lang> list>
    default this.Translate(globalVars, localVars) = translation {
        do! State.modify (fun context -> context.Copy())

        match expr with
        | DerivedPatterns.Lambdas (args, body) ->
            let args = List.collect id args
            let! translatedBody = this.TranslateBody(args, body)
            let! translatedArgs = this.TranslateArgs(args, globalVars, localVars)
            let! func = this.BuildFunction(translatedArgs, translatedBody)
            let! pragmas = this.GetPragmas()
            let! topLevelVarDecls = this.GetTopLevelVarDecls()

            return
                pragmas
                @ topLevelVarDecls
                @ [func]

        | _ -> return failwithf "Incorrect OpenCL quotation: %A" expr
    }

    override this.ToString() =
        sprintf "%A\n%A" var expr

type KernelFunc(var: Var, expr: Expr) =
    inherit Method(var, expr)

    override this.TranslateArgs(args, _, _) = translation {
        let! context = State.get

        let brahmaDimensionsTypes = [
            Range1D_
            Range2D_
            Range3D_
        ]

        return
            args
            |> List.filter
                (fun (variable: Var) ->
                    brahmaDimensionsTypes
                    |> (not << List.contains (variable.Type.Name.ToLowerInvariant()))
                )
            |> List.map
                (fun variable ->
                    let vType = Type.translate variable.Type |> State.eval context
                    let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                    if vType :? RefType<_> then
                        declSpecs.AddressSpaceQualifier <- Global

                    FunFormalArg(declSpecs, variable.Name)
                )
    }

    override this.BuildFunction(args, body) = translation {
        let retFunType = PrimitiveType Void :> Type<_>
        let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType, funQualifier = Kernel)
        return FunDecl(declSpecs, var.Name, args, body) :> ITopDef<_>
    }

type Function(var: Var, expr: Expr) =
    inherit Method(var, expr)

    override this.TranslateArgs(args, globalVars, localVars) = translation {
        let! context = State.get

        return
            args
            |> List.map (fun variable ->
                let vType = Type.translate variable.Type |> State.eval context
                let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                if
                    vType :? RefType<_> &&
                    globalVars |> List.contains variable.Name
                then
                    declSpecs.AddressSpaceQualifier <- Global
                elif
                    vType :? RefType<_> &&
                    localVars |> List.contains variable.Name
                then
                    declSpecs.AddressSpaceQualifier <- Local

                FunFormalArg(declSpecs, variable.Name)
            )
    }

    override this.BuildFunction(args, body) = translation {
        let! context = State.get

        let retFunType = Type.translate var.Type |> State.eval context
        let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType)
        let partAST =
            match retFunType with
            | :? PrimitiveType<Lang> as t when t.Type = Void -> body :> Statement<_>
            | _ -> this.AddReturn(body)

        return FunDecl(declSpecs, var.Name, args, partAST) :> ITopDef<_>
    }

type AtomicFunc(var: Var, expr: Expr, qual: AddressSpaceQualifier<Lang>) =
    inherit Method(var, expr)

    override this.TranslateArgs(args, globalVars, localVars) = translation {
        let! context = State.get

        let firstNonMutexIdx =
            args
            |> List.tryFindIndex (fun v -> not <| v.Name.EndsWith "Mutex")
            |> Option.defaultValue 0

        return
            args
            |> List.mapi
                (fun i variable ->
                    let vType = Type.translate variable.Type |> State.eval context
                    let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                    if i = firstNonMutexIdx then
                        declSpecs.AddressSpaceQualifier <- qual
                    elif
                        vType :? RefType<_> &&
                        globalVars |> List.contains variable.Name
                    then
                        declSpecs.AddressSpaceQualifier <- Global
                    elif
                        vType :? RefType<_> &&
                        localVars |> List.contains variable.Name
                    then
                        declSpecs.AddressSpaceQualifier <- Local

                    FunFormalArg(declSpecs, variable.Name)
                )
    }

    override this.BuildFunction(args, body) = translation {
        let! context = State.get

        let retFunType = Type.translate var.Type |> State.eval context
        let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType)
        let partAST = this.AddReturn body

        return FunDecl(declSpecs, var.Name, args, partAST) :> ITopDef<_>
    }
