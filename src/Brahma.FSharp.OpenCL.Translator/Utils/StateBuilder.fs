namespace Brahma.FSharp.OpenCL.Translator

type State<'state, 'result> = State of ('state -> 'result * 'state)

module State =
    let run state (State f) =
        f state

    let exec state (State f) =
        snd (f state)

    let eval state (State f) =
        fst (f state)

    let return' x = State <| fun state ->
        (x, state)

    let (>>=) x f = State <| fun state ->
        let (y, state') = run state x
        run state' (f y)

    let get = State (fun s -> s, s)

    let put newState = State <| fun _ ->
        (), newState

    // modify state
    let modify f =
        get >>= (f >> put)

    // apply f to state to produce value
    let gets f =
        get >>= (f >> return')

    let map f s = State <| fun state ->
        let (x, state) = run state s
        f x, state

    let using f x = State <| fun state ->
        eval (f state) x, state

    let collect (list: State<'s, 'a> list) =
        list
        |> List.fold
            (fun state elem ->
                state >>= fun state ->
                elem >>= fun elem ->
                return' (elem :: state)
            ) (return' List.empty)
        |> fun args -> map List.rev args

type StateBuilder<'state>() =
    member this.Bind(x: State<'state, 'a>, f: 'a -> State<'state, 'b>) = State.(>>=) x f
    member this.Return(x: 'a) : State<'state, 'a> = State.return' x
    member this.ReturnFrom(x: State<'state, 'a>) = x
    member this.Zero() : State<'state, unit> = State.return' ()

    member this.Combine(x1: State<'state, _>, x2: State<'state, _>) =
        State <| fun context ->
            let (_, context) = State.run context x1
            State.run context x2

    member this.Delay(rest) =
        this.Bind(this.Zero(), (fun () -> rest ()))

    member this.For(seq, f) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine(x1, x2))

    member this.While(f, x) =
        if f () then
            this.Combine(x, this.While(f, x))
        else this.Zero()
