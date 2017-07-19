module forHopacTests

open Brahman.Substrings.Helpers
open Brahman.Substrings.Matcher

let matcher2 = new Brahman.Substrings.MatcherHopac.Matcher(1UL * 1024UL * 1024UL)
let template = [|1uy;1uy|]
let zSeq  = seq {for i in 0 .. 10000 do if i = 0 then yield! template else yield 0uy}  
printfn "%A" zSeq     

let matcher = new Brahman.Substrings.Matcher.Matcher(1UL * 1024UL * 1024UL)
let res2 = matcher.RabinKarp (zSeq,[|template|])

printfn "%i" res2.Data.Length              //1
printfn "%i" res2.Data.[0].ChunkNum        //0
printfn "%i" res2.Data.[0].Offset          //0
printfn "%i" res2.Data.[0].PatternId       //0



let res1 = matcher2.RabinKarp (zSeq,[|template|])
printfn "%i" res1.Data.Length              //1
printfn "%i" res1.Data.[0].ChunkNum        //0
printfn "%i" res1.Data.[0].Offset          //0
printfn "%i" res1.Data.[0].PatternId       //0


System.Console.ReadKey(true) |> ignore

    
//    [<Test>]
//    member this.``RabinKarpHopac two patterns 1.`` () = 
//        let templates = [|[|1uy;1uy|];[|2uy;2uy|]|]
//        let zSeq  = seq {for i in 0 .. 10000 do if i = 10 then yield! templates.[1] else yield 0uy}        
//        let res = matcher2.RabinKarp (zSeq,templates)
//        Assert.AreEqual(res.Data.Length,1)
//        Assert.AreEqual(res.Data.[0].ChunkNum,0)
//        Assert.AreEqual(res.Data.[0].Offset,10)
//        Assert.AreEqual(res.Data.[0].PatternId, Array.findIndex ((=)templates.[1]) res.Templates )
//
//    [<Test>]
//    member this.``RabinKarpHopac two patterns 2.`` () = 
//        let templates = [|[|1uy;1uy|];[|2uy;2uy|]|]
//        let zSeq  = seq {for i in 0 .. 10000 do if i = 10 then yield! templates.[1] elif i = 19 then yield! templates.[0] else yield 0uy}        
//        let res = matcher2.RabinKarp (zSeq,templates)
//        Assert.AreEqual(res.Data.Length,2)
//        Assert.AreEqual(res.Data.[0].ChunkNum,0)
//        Assert.AreEqual(res.Data.[0].Offset,10)
//        Assert.AreEqual(res.Data.[1].Offset,20)
//        Assert.AreEqual(res.Data.[0].PatternId, Array.findIndex ((=)templates.[1]) res.Templates)
//        Assert.AreEqual(res.Data.[1].PatternId, Array.findIndex ((=)templates.[0]) res.Templates)
//
//
//    [<Test>]
//    member this.``RabinKarpHopac two chanks 1`` () = 
//        let template = [|1uy;1uy|]
//        let x = 1024 * 1024 / 3 + 5000
//        let zSeq  = seq {for i in 0 .. 1024 * 1024 do if i = x  then yield! template else yield 0uy}        
//        let res = matcher2.RabinKarp (zSeq,[|template|])
//        Assert.AreEqual(res.Data.Length,1)
//        Assert.AreEqual(res.Data.[0].ChunkNum,1)
//        Assert.AreEqual(x - res.ChunkSize + 32, res.Data.[0].Offset)
//        Assert.AreEqual(res.Data.[0].PatternId,0)