module ExpectoTemplate

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv

// [<Tests>]
// let allTests =
//     testList "All tests" [
//         Atomic.tests
//     ]
//     |> testSequenced

// [<EntryPoint>]
// let main argv =
//     allTests
//     |> runTestsWithCLIArgs [] argv
