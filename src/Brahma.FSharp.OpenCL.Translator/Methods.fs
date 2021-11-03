namespace rec Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST

[<AbstractClass>]
type Method(var: Var, expr: Expr, context: TargetContext<Lang,Statement<Lang>>) =
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

    abstract TranslateBody : Var list * Expr -> StatementBlock<Lang> * TargetContext<Lang, Statement<Lang>>
    default this.TranslateBody(args, body) =
        let (b, context) =
            let clonedContext = context.Clone()

            clonedContext.Namer.LetIn()
            args |> List.iter (fun v -> clonedContext.Namer.AddVar v.Name)

            Body.translate body clonedContext

        match b with
        | :? StatementBlock<Lang> as sb -> sb
        | :? Statement<Lang> as s -> StatementBlock <| ResizeArray [s]
        | _ -> failwithf "Incorrect function body: %A" b
        , context

    abstract TranslateArgs : Var list * string list * string list * TargetContext<Lang, Statement<Lang>> -> FunFormalArg<Lang> list

    abstract BuildFunction : FunFormalArg<Lang> list * StatementBlock<Lang> * TargetContext<Lang, Statement<Lang>> -> ITopDef<Lang>

    abstract GetPragmas : TargetContext<Lang, Statement<Lang>> -> ITopDef<Lang> list
    default this.GetPragmas(context) =
        let pragmas = ResizeArray()

        if context.Flags.enableAtomic then
            pragmas.Add(CLPragma CLGlobalInt32BaseAtomics :> ITopDef<_>)
            pragmas.Add(CLPragma CLLocalInt32BaseAtomics :> ITopDef<_>)

        if context.Flags.enableFP64 then
            pragmas.Add(CLPragma CLFP64)

        List.ofSeq pragmas

    abstract GetTopLevelVarDecls : TargetContext<Lang, Statement<Lang>> -> ITopDef<Lang> list
    default this.GetTopLevelVarDecls(context) =
        context.TopLevelVarsDeclarations
        |> Seq.cast<_>
        |> List.ofSeq

    abstract GetTranslatedTuples : TargetContext<Lang, Statement<Lang>> -> ITopDef<Lang> list
    default this.GetTranslatedTuples(context) =
        context.TupleList
        |> Seq.cast<_>
        |> List.ofSeq

    abstract Translate : string list * string list * ITopDef<Lang> list -> ITopDef<Lang> list
    default this.Translate(globalVars, localVars, translatedTypes) =
        match expr with
        | DerivedPatterns.Lambdas (args, body) ->
            let args = List.collect id args
            let (translatedBody, context) = this.TranslateBody(args, body)
            let translatedArgs = this.TranslateArgs(args, globalVars, localVars, context)
            let func = this.BuildFunction(translatedArgs, translatedBody, context)
            let pragmas = this.GetPragmas(context)
            let topLevelVarDecls = this.GetTopLevelVarDecls(context)
            let translatedTuples = this.GetTranslatedTuples(context)

            pragmas
            @ translatedTuples
            @ topLevelVarDecls
            @ translatedTypes
            @ [func]

        | _ -> failwithf "Incorrect OpenCL quotation: %A" expr

    override this.ToString() =
        sprintf "%A\n%A" var expr

type KernelFunc(var: Var, expr: Expr, context: TargetContext<Lang,Statement<Lang>>) =
    inherit Method(var, expr, context)

    override this.TranslateArgs(args, _, _, context) =
        let brahmaDimensionsTypes = [
            Range1D_
            Range2D_
            Range3D_
        ]

        args
        |> List.filter
            (fun (variable: Var) ->
                brahmaDimensionsTypes
                |> (not << List.contains (variable.Type.Name.ToLowerInvariant()))
            )
        |> List.map
            (fun variable ->
                let vType = Type.translate variable.Type true None context
                let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                if vType :? RefType<_> then
                    declSpecs.AddressSpaceQualifier <- Global

                FunFormalArg(declSpecs, variable.Name)
            )

    override this.BuildFunction(args, body, _) =
        let retFunType = PrimitiveType Void :> Type<_>
        let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType, funQualifier = Kernel)
        FunDecl(declSpecs, var.Name, args, body) :> ITopDef<_>

type Function(var: Var, expr: Expr, context: TargetContext<Lang,Statement<Lang>>) =
    inherit Method(var, expr, context)

    override this.TranslateArgs(args, globalVars, localVars, context) =
        args
        |> List.map (fun variable ->
            let vType = Type.translate variable.Type true None context
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

    override this.BuildFunction(args, body, context) =
        let retFunType = Type.translate var.Type false None context
        let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType)
        let partAST =
            if (retFunType :?> PrimitiveType<_>).Type <> Void then
                this.AddReturn(body)
            else
                body :> Statement<_>

        FunDecl(declSpecs, var.Name, args, partAST) :> ITopDef<_>

type AtomicFunc(var: Var, expr: Expr, qual: AddressSpaceQualifier<Lang>, context: TargetContext<Lang,Statement<Lang>>) =
    inherit Method(var, expr, context)

    override this.TranslateArgs(args, globalVars, localVars, context) =
        let firstNonMutexIdx =
            args
            |> List.tryFindIndex (fun v -> not <| v.Name.EndsWith "Mutex")
            |> Option.defaultValue 0

        args
        |> List.mapi
            (fun i variable ->
                let vType = Type.translate variable.Type true None context
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

    override this.BuildFunction(args, body, context) =
        let retFunType = Type.translate var.Type false None context
        let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType)
        let partAST = this.AddReturn body

        FunDecl(declSpecs, var.Name, args, partAST) :> ITopDef<_>
