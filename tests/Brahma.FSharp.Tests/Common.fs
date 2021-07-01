namespace Brahma.FSharp.Tests

open System.IO
open Brahma.OpenCL
open Expecto
open Expecto
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.WorkflowBuilder

module CustomDatatypes =
    // TODO заменить рекорд на структуру ??
    [<Struct>]
    type WrappedInt = { InnerValue: int }
    with
        static member (+) (x: WrappedInt, y: WrappedInt) = { InnerValue = x.InnerValue + y.InnerValue }
        static member (-) (x: WrappedInt, y: WrappedInt) = { InnerValue = x.InnerValue - y.InnerValue }
        static member Wrap (x: int) = { InnerValue = x }

module Utils =
    let context = OpenCLEvaluationContext()

    let getValidGlobalSize wgSize neededSize =
        (neededSize + wgSize - 1) / wgSize * wgSize

    let filesAreEqual file1 file2 =
        let all1 = (File.ReadAllText file1).Trim().Replace("\r\n", "\n")
        let all2 = (File.ReadAllText file2).Trim().Replace("\r\n", "\n")
        Expect.equal all1 all2 "Files should be equals as strings"

    let platformMessage (provider: ComputeProvider) testName =
        printfn "Run %s on %A" testName provider


