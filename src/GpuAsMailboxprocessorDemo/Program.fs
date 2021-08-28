[<EntryPoint>]
let main argv =
    printfn "Started!"
    let host = new Brahma.FSharp.OpenCL.Host()
    let res = host.Do(argv)
    printfn "res = %A" res
    0 // return an integer exit code
