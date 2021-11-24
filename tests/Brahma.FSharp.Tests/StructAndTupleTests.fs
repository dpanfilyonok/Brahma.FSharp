module StructAndTuple

open Expecto
open Brahma.FSharp.OpenCL
open FSharp.Quotations
open Brahma.FSharp.Tests

[<Struct>]
type RecordOfIntInt64 =
    {
        X: int
        Y: int64
    }

[<Struct>]
type RecordOfBoolBool =
    {
        X: bool
        Y: bool
    }

[<Struct>]
type GenericRecord<'a, 'b> =
    {
        mutable X: 'a
        mutable Y: 'b
    }

let check<'a when 'a : struct and 'a : equality> (data: 'a[]) (command: int -> Expr<Range1D -> ClArray<'a> -> unit>) =
    let length = data.Length

    let expected = data

    let actual =
        opencl {
            use! buffer = ClArray.toDevice data
            do! runCommand (command length) <| fun it ->
                it
                <| Range1D.CreateValid(data.Length, 256)
                <| buffer

            return! ClArray.toHost buffer
        }
        |> ClTask.runSync context

    ""
    |> Expect.sequenceEqual actual expected

let tupleTestCases = testList "d" [
    let inline command length =
        <@
            fun (gid: int) (buffer: clarray<struct('a * 'b)>) ->
                if gid < length then
                    let struct(a, b) = buffer.[gid]
                    buffer.[gid] <- struct(a, b)
        @>

    testProperty "1" <| fun (data: struct(int * int)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "2" <| fun (data: struct(int * int64)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "3" <| fun (data: struct(bool * bool)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "4" <| fun (data: struct((int * int) * (int * int))[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "5" <| fun (data: struct((int * int64) * (bool * bool))[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "6" <| fun (data: struct(RecordOfIntInt64 * RecordOfBoolBool)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "7" <| fun (data: struct(GenericRecord<int, int64> * GenericRecord<bool, bool>)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "5.1" <| fun (data: struct(int * int64 * bool)[]) ->
        if data.Length <> 0 then
            check data <| fun length ->
                <@ fun (range: Range1D) (buffer: ClArray<_>) ->
                    let gid = range.GlobalID0
                    if gid < length then
                        let struct(a1, a2, a3) = buffer.[gid]
                        buffer.[gid] <- struct(a1, a2, a3)
                @>

    testProperty "8" <| fun (data: struct(int * int * int * int * int * int * int * int * int * int)[]) ->
        if data.Length <> 0 then
            check data <| fun length ->
                <@ fun (range: Range1D) (buffer: ClArray<_>) ->
                    let gid = range.GlobalID0
                    if gid < length then
                        let struct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = buffer.[gid]
                        buffer.[gid] <- struct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
                @>

    testProperty "9" <| fun (data: struct((int * int) * (int * int))[]) ->
        if data.Length <> 0 then
            check data <| fun length ->
                <@ fun (range: Range1D) (buffer: ClArray<_>) ->
                    let gid = range.GlobalID0
                    if gid < length then
                        let struct((a, b), (c, d)) = buffer.[gid]
                        buffer.[gid] <- struct((a, b), (c, d))
                @>
]

let structTestCases = testList "" [
    let inline command length =
        <@
            fun (gid: int) (buffer: clarray<GenericRecord<'a, 'b>>) ->
                if gid < length then
                    let { X = x; Y = y } = buffer.[gid]
                    let mutable innerStruct = { X = x; Y = y }
                    innerStruct.X <- x
                    innerStruct.Y <- y
                    buffer.[gid] <- { X = innerStruct.X; Y = innerStruct.Y }
        @>

    testProperty "2" <| fun (data: GenericRecord<int, bool>[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty "2" <| fun (data: GenericRecord<(int * float), (bool * bool)>[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)
]

let tests =
    testList "Tests structs and tuples" [
        tupleTestCases
        structTestCases
    ]
    |> testSequenced
