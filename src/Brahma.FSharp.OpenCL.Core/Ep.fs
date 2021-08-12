module X

[<EntryPoint>]
let main a =
    let h = new Host()
    let r = h.Do()
    printfn "res = %A" r
    0
