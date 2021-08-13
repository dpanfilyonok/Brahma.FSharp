module X

[<EntryPoint>]
let main a =
    let h = new Brahma.FSharp.OpenCL.Host()
    let r = h.Do()
    printfn "res = %A" r
    0
