module ExpectoTemplate

open Expecto

[<EntryPoint>]
let main argv =
    let config = { defaultConfig with parallel=false }
    Tests.runTestsInAssembly config argv

