module Brahma.FSharp.OpenCL.Workflow.BasicMonads.State

type State<'state, 'result> =
    State of ('state -> 'result * 'state)

let runState (State s) = s

let execState state = fst << runState state

let evalState state = snd << runState state

let ignoreResult (state: State<'s, 'a>) =
    State <| fun st ->
        let _, new_st = runState state st
        ((), new_st)

let getState = State (fun st -> st, st)

let putState st = State (fun _ -> ((), st))

type StateBuilder<'state>() =
    abstract member Return : 'a -> State<'state, 'a>

    abstract member ReturnFrom : State<'state, 'a> -> State<'state, 'a>

    abstract member Bind : State<'state,'a> * ('a -> State<'state,'b>) -> State<'state,'b>

    abstract member Zero : unit -> State<'state, unit>

    abstract member TryWith : State<'state,'a> * (exn -> State<'state,'a>) -> State<'state,'a>

    default this.Return x =
        State <| fun st -> (x, st)

    default this.ReturnFrom m = m

    default this.Bind (m, k) =
        State <| fun st ->
            let (res, st) = runState m st
            runState <| k res <| st

    default this.Zero () =
        this.Return ()

    default this.TryWith(tryBlock, handler) =
         State <| fun st ->
         try
             runState tryBlock st
         with
         | e ->
             runState (handler e) st
