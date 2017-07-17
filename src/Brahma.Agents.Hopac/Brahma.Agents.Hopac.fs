module Brahma.Agents.Hopac

open Brahma.Helpers
open OpenCL.Net
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.OpenCL
open Hopac
open Hopac.Infixes
open Hopac.Extensions

type GpuConfig =
    val Name: string
    val Workers: int
    new (n,w) = {Name=n; Workers=w}

type WorkerConfig =
    val AdditionalBuffsNum: uint32
    val GpuCommandQueue: CommandQueue
    val GPUProvider: ComputeProvider
    new (bNum,queue,provider) = {AdditionalBuffsNum = bNum; GpuCommandQueue = queue; GPUProvider = provider}

type Msg<'data,'res> =
    | Die of IVar<unit>
    | Process of 'data*('res -> unit)
    | PostProcess of 'res
    | Fill of 'data*(Option<'data> -> unit)
    | InitBuffers of array<'data>*IVar<array<'data>>
    | Get of IVar<Option<'data>>
    | Enq of 'data

type Reader<'d>  (fillF:'d -> Option<'d>) = 

    member this.Read inCh (a, cont) = inCh *<- (Fill(a, cont))     
    member this.Die (inCh : Ch<_>) = inCh *<+=>- Die :> Job<_> // здесь и подобных были таймауты... могут понадобиться
    
    member this.Create : Job<Ch<Msg<'d,_>>>= job {
        let inCh = Ch()
        let rec loop n = job {
            let! msg = Ch.take inCh
            match msg with
            | Die ch -> 
                do! ch *<= () 
            | Fill (x, cont) ->
                let filled = fillF x
                cont filled
                if filled.IsNone
                then this.Die(inCh) |> ignore
                return! loop n
            | x -> 
                    printfn "unexpected message for reader: %A" x
                    return! loop n 
            }
        do! Job.start (loop 0)
        return inCh
        }



type Worker<'d,'r>(f: 'd -> 'r) =
    member this.Create = job {
        let inCh = Ch()
        let rec loop n = job {
            let! msg = Ch.take inCh
            match msg with
            | Die ch ->
                do! ch *<= ()
            | Process (x,continuation) ->
                let r = f x
                continuation r
                return! loop n
            | x -> 
                printfn "unexpected message for Worker: %A" x
                return! loop n
            }
        do! Job.start (loop 0)
        return inCh
        }
 
    member this.Process inCh (a, continuation) = inCh *<- (Process(a,continuation))
    member this.Die(inCh) = inCh *<+=>- Die :> Job<_>
    
type DataManager<'d>(reader:Reader<'d>) =

    member this.Create = job {
        let inCh = Ch()
        let dataToProcess = new System.Collections.Concurrent.ConcurrentQueue<Option<'d>>()
        let dataToFill = new System.Collections.Generic.Queue<_>()
        let dataIsEnd = ref false
        let f a =  dataToProcess.Enqueue a
                   dataIsEnd := Option.isNone a
        let rec loop n = job {
            let cnt = ref 3
            while dataToFill.Count > 0 && !cnt > 0 do
                decr cnt                            
                let b = dataToFill.Dequeue()
                if not <| !dataIsEnd
                then 
                    let! rdr = reader.Create
                    do! reader.Read rdr (b, fun a -> f a)
            let! msg = Ch.take inCh
            match msg with
            | Die ch ->
                if !dataIsEnd 
                then
                    do! ch *<= ()
                else 
                    do! inCh *<- Die ch
                    return! loop n
            | InitBuffers (bufs,ch) ->
                do! ch *<= bufs
                bufs |> Array.iter dataToFill.Enqueue                                
                return! loop n
            | Get ch -> 
                let s,r = dataToProcess.TryDequeue()
                if s
                then 
                    if r.IsNone then dataIsEnd := true
                    do! ch *<= r
                elif not !dataIsEnd
                then do! inCh *<- Get ch
                else do! ch *<=  None
                return! loop n
            | Enq b -> 
                dataToFill.Enqueue b
                return! loop n
            | x ->  
                printfn "Unexpected message for Worker: %A" x
                return! loop n
        }
        do! Job.start (loop 0)
        return inCh
    }

    member this.InitBuffers inCh bufs = inCh *<+=>- (fun reply -> InitBuffers(bufs, reply)) :> Job<_>
    member this.Enq inCh b = inCh *<- Enq b
    member this.GetData inCh = inCh *<+=>- (fun reply -> Get reply) :> Job<_>
    member this.Die inCh = inCh *<+=>- Die :> Job<_>


type Master<'d,'r,'fr>(workers:array<Worker<'d,'r>>, fill: 'd -> Option<'d>, bufs:ResizeArray<'d>, postProcessF:Option<'r->'fr>) =             
    
    let isEnd = ref false
    let isDataEnd = ref false
    let reader = new Reader<'d>(fill)
    let dataManager = new DataManager<'d>(reader)
    let postprocessor = postProcessF |> Option.map(fun f -> new Worker<_,_>(f))
    let freeWorkers = new System.Collections.Concurrent.ConcurrentQueue<_>(workers)
    
    member this.Create = job {     
        let inCh = Ch()
        let bufers = dataManager.InitBuffers inCh (bufs.ToArray())
        let rec loop n = job {
            
            let! dmngrCh = dataManager.Create
            if not freeWorkers.IsEmpty
            then 
                let success,w = freeWorkers.TryDequeue()
                if success
                then
                    let! b = dataManager.GetData dmngrCh
                    if b.IsSome
                    then
                        let! wCh = w.Create
                        if postprocessor.IsSome 
                        then
                            let! ppCh = postprocessor.Value.Create
                            do! w.Process wCh
                                    (b.Value
                                    , fun a ->
                                          postprocessor.Value.Process ppCh (a, fun _ -> ()) |> ignore 
                                          freeWorkers.Enqueue w
                                          dataManager.Enq dmngrCh b.Value |> ignore) // и тут игнор
                        else 
                            do! w.Process wCh
                                    (b.Value
                                    , fun a ->
                                          freeWorkers.Enqueue w
                                          dataManager.Enq dmngrCh b.Value |> ignore)
                    else 
                        isDataEnd := true
            let! msg = Ch.take inCh
            match msg with
            | Die ch ->
                do! dataManager.Die dmngrCh 
                for i = 0 to workers.Length - 1 do
                    let! iwCh = workers.[i].Create                       
                    do! workers.[i].Die iwCh 
                if postprocessor.IsSome 
                then
                    let! ppCh = postprocessor.Value.Create
                    postprocessor.Value.Die ppCh |> ignore 
                isEnd := true
                do! ch *<= ()
            | x ->
                printfn "unexpected message for Worker: %A" x
                return! loop n 
            }
        do! Job.start (loop 0)
        return inCh
    }

    member this.IsDataEnd = !isDataEnd
    member this.Die inCh c = inCh *<+=>- Die :> Job<_>