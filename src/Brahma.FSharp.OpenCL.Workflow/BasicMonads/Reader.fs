module Brahma.FSharp.OpenCL.Workflow.BasicMonads.Reader

type Reader<'env, 'a> =
    Reader of ('env -> 'a)

let runReader (Reader r) = r

let ask = Reader (fun env -> env)

type ReaderBuilder<'env>() =
    abstract member Return : 'a -> Reader<'env, 'a>

    abstract member ReturnFrom : Reader<'env, 'a> -> Reader<'env, 'a>

    abstract member Bind : Reader<'env, 'a> * ('a -> Reader<'env, 'b>) -> Reader<'env, 'b>

    abstract member Zero : unit -> Reader<'env, unit>

    abstract member TryWith : Reader<'env, 'a> * (exn -> Reader<'env, 'a>) -> Reader<'env, 'a>

    default this.Return x =
        Reader <| fun _ -> x

    default this.ReturnFrom m = m

    default this.Bind(m, k) =
        Reader <| fun env ->
            let res = runReader m env
            runReader <| k res <| env

    default this.Zero () =
        this.Return ()

    default this.TryWith (tryBlock, handler) =
        Reader <| fun env ->
            try
                runReader tryBlock env
            with
            | e ->
                runReader (handler e) env
