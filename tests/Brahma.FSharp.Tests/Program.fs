open System.Runtime.InteropServices
open System.Runtime.Serialization
open Expecto
open Brahma.FSharp


[<Struct>]
type GenericStruct<'a, 'b> =
    val mutable X: 'a
    val mutable Y: 'b
    new(x, y) = { X = x; Y = y }


[<Struct>]
type GenericRecord<'a, 'b> =
    {
        mutable X: 'a
        mutable Y: 'b
    }

[<Tests>]
let allTests =
    testList "All tests" [
        testList "Translation tests" TranslationTests.tests
        testList "Execution tests" ExecutionTests.tests
    ]
    |> testSequenced

[<EntryPoint>]
let main argv =
    let a =
        <@
            fun (range: Range1D) (acc: int clcell) ->
                acc.Value <- 5
        @>
//    TranslatorTests.Helpers.openclTranslate TranslationTests.translators.Head a
//    |> printfn "%A"
////    0

//    let instance = FormatterServices.GetUninitializedObject(typeof<GenericRecord<int, int64>>);
//    GCHandle.Alloc(instance, GCHandleType.Pinned).Free();
    opencl {
        let! cell = ClCell.toDevice 5
        do! runCommand a <| fun kernel ->
            kernel
            <| Range1D.CreateValid(10, 256)
            <| cell

        return! ClCell.toHost cell
    }
    |> ClTask.runSync (RuntimeContext(ClDevice.GetFirstAppropriateDevice()))
    |> printfn "%A"
    0
//    allTests
//    |> runTestsWithCLIArgs [] argv
