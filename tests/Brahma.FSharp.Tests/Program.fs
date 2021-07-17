open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.Tests
open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open FSharp.Reflection
open System.Reflection

// TODO format tests
// [<EntryPoint>]
// let main argv =
//     Tests.runTestsInAssembly defaultConfig argv

// [<Tests>]
// let allTests =
//     testList "All tests" [
//         Translator.tests
//         Atomic.tests
//     ]
//     |> testSequenced

// [<EntryPoint>]
// let main argv =
//     allTests
//     |> runTestsWithCLIArgs [] argv

[<EntryPoint>]
let main argv =
    let e =
        <@
            fun (range: _1D) (array: int[]) ->
                let g x = x + 2
                array.[0] <- g 4
        @>

    // let a (e: Expr) =
    //     match e with
    //     | ShapeLambda (v, expr) -> printfn "yes"
    //     | Lambdas (v, body) ->
    //         printfn "%A %A" v body
    //     | SpecificCall <@ (+) @> (_, t, args) -> printfn "%A" t

    let kernel =
        <@
            fun (range: _1D) (array: int[]) ->
                // TODO что-то не то с тем, что парамет атомарной функции должнен быть указателем
                let g x y = atomic (+) x y
                let a = g array.[0] 5
                array.[0] <- 1
                // () -- error
        @>

    let kernel2 =
        <@
            fun (range: _1D) (array: int[]) ->
                // NOTE тут все норм
                let a = atomic (/) array.[0] 5
                array.[0] <- 1
                // () -- error
        @>

    let kernel3 =
        <@
            fun (range: _1D) (array: int[]) ->
                while atomic (+) array.[0] 6 = 7 do
                    array.[0] <- 1
                array.[0] <- 1
        @>

    let command =
        <@ fun (range: _1D) (buf: array<int>) ->
            let localA = localArray<int> 5
            let f (y: int) =
                y + 5

            let b = f localA.[0]
            let a = f buf.[0]

            buf.[0] <- a
        @>

    let command2 =
        <@ fun (range: _1D) (buf: array<int>) ->
            let localA = localArray<int> 5
            let f y =
                let Argi index = if index = 0 then localA.[1] else buf.[2]
                Argi y

            buf.[0] <- f 0
        @>

    let k4 =
        <@
            // не работает, тк у лямбды больше аргументов
            fun (range: _1D) (buf: array<int>) ->
                let a = atomic (fun x y -> x + 1) buf.[0]
                buf.[0] <- 0
        @>

    let k4 =
        <@
            fun (range: _1D) (buf: array<int>) ->
                let a = atomic (fun x -> x / 2) buf.[0]
                buf.[0] <- 0
        @>

    // let assembly = System.AppDomain.CurrentDomain.GetAssemblies()

    // let makeLambdaType domain range =
    //     FSharpType.MakeFunctionType(domain, range)

    // let lambdaType =
    //     lambdaArgs
    //     |> List.collect id
    //     |> List.map (fun var -> var.Type)
    //     |> fun args -> args @ [args.[0]]
    //     |> List.reduceBack makeLambdaType

    // printfn "%A" <| AppDomain.CurrentDomain.GetAssemblies()
    // printfn "%A" <| (AppDomain.CurrentDomain.GetAssemblies() |> Seq.map (fun ass -> ass.FullName))

    // let s = OpenCL.
    // let assembly =
    //     System.AppDomain.CurrentDomain.GetAssemblies()
    //     |> Array.find (fun ass -> ass.FullName.Contains "Brahma.FSharp.OpenCL.Extensions")

    // // printfn "%A" <| assembly.GetTypes()

    // let functions =
    //     assembly.GetTypes()
    //     |> Array.find (fun t -> t.Name = "OpenCL")
    //     |> fun t -> t.GetMethods()
    //     |> Seq.find (fun mi -> mi.Name = "atomic")
    //     |> fun mi ->
    //         mi.MakeGenericMethod(
    //             // lambdaArgs
    //             // |> List.collect id
    //             // |> List.map (fun var -> var.Type)
    //             // |> fun args -> (args @ [args.[0]]).Tail
    //             // |> fun types -> types.Head :: [(List.reduceBack makeLambdaType types.Tail)]
    //             // |> List.toArray
    //             typeof<int>,
    //             typeof<int->int>
    //         )

    printfn "%A" <| kernel2
    printfn "%A" <| Transformer.quotationTransformer kernel2 []
    printfn "%A" <| Utils.openclTranslate kernel2
    // printfn "%A" <| functions
    // a e
    0
