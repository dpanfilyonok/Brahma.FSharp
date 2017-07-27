module Brahman.Substrings.MatcherCS

[<Struct>]
type ReadingResult =
    val Data : array<byte>
    val IsEndOfRead : bool
    new (data,isEndOfRead) = {Data=data; IsEndOfRead = isEndOfRead}

type IHDReader = 
    abstract member Read: array<byte> -> ReadingResult


type MatcherCS () =
    let matcher = Matcher.Matcher()
    member this.RabinKarp (f, (reader:IHDReader), templates) =
        let readFun =
            fun buf -> 
                let res = reader.Read buf
                if res.IsEndOfRead then None else Some res.Data
        matcher.RabinKarp(f, readFun, templates)

