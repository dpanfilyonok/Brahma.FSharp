open Expecto

open Brahma.FSharp.Tests
open Brahma.FSharp.OpenCL
open FSharp.Quotations

[<Tests>]
let allTests =
    testList "All tests" [
        Full.tests
        // Workflow.tests
        // Translator.tests
        // QuotationTransformers.tests
        // CompositeTypesTests.tests
        // Union.tests
        // Atomic.tests
    ]
    |> testSequenced

type A<'a> = | S of 'a | N | K of 'a

[<EntryPoint>]
let main argv =
    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let tempVarX = 1
                max buf.[0] tempVarX |> ignore
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let tempVarX = 1
                buf.[0] <- max buf.[0] tempVarX
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let tempVarY = 1
                buf.[0] <- max buf.[0] tempVarY
                buf.[0] <- max buf.[0] tempVarY
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let gid = range.GlobalID0
                buf.[gid] <- max (atomic (fun x -> x + 1) buf.[gid]) (if 1 > 0 then 100 else -100)
        @>

    let kernel =
        <@
            fun (range: Range1D) (buf: int clarray) ->
                let gid = range.GlobalID0
                atomic (fun x -> x + 1) buf.[gid] |> ignore
        @>

    // let kernel =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             let tempVarY = 1.
    //             buf.[0] <- atomic (+) buf.[0] tempVarY
    //     @>

    // let kernel =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             while true do
    //                 buf.[0] <- 1.
    //     @>

    // let kernel =
    //     <@
    //         fun (range: Range1D) (buf: float clarray) ->
    //             let f (x: int) =
    //                 x |> ignore
    //                 x

    //             f 6
    //     @>

    // opencl {
    //     do! runCommand kernel (fun kernelPrepare -> kernelPrepare <| Range1D(10))
    // }
    // |> ClTask.runSync context
    // Utils.openclTransformQuotation kernel
    // |> printfn "%A"
    // Utils.openclTranslate kernel
    // |> printfn "%A"
    // let rnd = System.Random()
    // let input1 = Array.init 100_000 (fun i -> rnd.Next())
    // let input2 = Array.init 100_000 (fun i -> rnd.Next())
    // let inputArrayLength = input1.Length
    // let add (op:Expr<Option<int> -> Option<int> -> Option<int>>) =
    //     <@
    //         fun (ndRange: Range1D)
    //             (input1: int clarray)
    //             (input2: int clarray)
    //             (output: int clarray) ->

    //             let i = ndRange.GlobalID0
    //             if i < inputArrayLength then
    //                 let x = if input1.[i] < 0 then None else Some input1.[i]
    //                 let y = if input2.[i] < 0 then None else Some input1.[i]
    //                 output.[i] <- match (%op) x y with Some x -> x | None -> 0
    //     @>

    // // let actual =
    // //     opencl {
    // //         use! input1 = ClArray.toDevice input1
    // //         use! input2 = ClArray.toDevice input2
    // //         use! output = ClArray.alloc<int> 100_000
    // //         let op = <@ fun x y ->
    // //                         match x with
    // //                             S x -> match y with S y -> S (x + y) | N -> S x
    // //                         | N -> match y with S y -> S y | N -> N  @>
    // //         do! runCommand (add (op)) <| fun x ->
    // //             x
    // //             <| Range1D.CreateValid(input1.Length, 256)
    // //             <| input1
    // //             <| input2
    // //             <| output

    // //         return! ClArray.toHost output
    // //     }
    // //     |> ClTask.runSync context
    // let op = <@ fun x y ->
    //             match x with
    //             Some x -> match y with Some y -> Some (x + y) | None -> Some x
    //             | None -> match y with Some y -> Some y | None -> None  @>
    // Utils.openclCompile (add op)
    // |> printfn "%A"
    // 0
    allTests
    |> runTestsWithCLIArgs [] argv
