module Brahma.FSharp.Tests.Common

open System.IO
open Brahma.FSharp.OpenCL
open Expecto

let filesAreEqual file1 file2 =
        let all1 = (File.ReadAllText file1).Trim().Replace ("\r\n", "\n")
        let all2 = (File.ReadAllText file2).Trim().Replace ("\r\n", "\n")
        Expect.equal all1 all2 "Files should be equals as strings"

let platformMessage (gpu: GPU) testName =
    printfn "Run %s on %A" testName gpu.ClDevice
