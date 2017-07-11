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

type Reader<'d, 'r> = 
    val fillF : 'd -> Option<'d>
    val readerReqCh : Ch<Msg<'d,'r>>
    val readerReplyCh : Ch<'d>

    new (f) = {fillF = f; readerReqCh = Ch(); readerReplyCh = Ch()} 

    member this.Job = job {
        let rec loop n = job {
            let! msg = Ch.take this.readerReqCh
            match msg with
            | Die ch -> 
                do! ch *<= () 
            | Fill (x, cont) ->
                let filled = this.fillF x
                cont filled
                if filled.IsNone
                then this.Die() |> ignore 
                return! loop n
            | x -> 
                 printfn "unexpected message for reader: %A" x
                 return! loop n 
            }
        do! Job.start (loop 0)
        return this.readerReqCh, this.readerReplyCh
        }

    member this.Read(a, cont) = this.readerReqCh *<- (Fill(a, cont))     
    member this.Die() = this.readerReqCh *<+=>- Die :> Job<_>

type Worker<'d,'r> =
    val func : 'd -> 'r
    val workerReqCh : Ch<Msg<'d,'r>>
    val workerReplyCh : Ch<'d>

    new (f) = {func = f; workerReqCh = Ch(); workerReplyCh = Ch()} 

    member this.Job = job {
        let rec loop n = job {
            let! msg = Ch.take this.workerReqCh
            match msg with
            | Die ch ->
                do! ch *<= ()
            | Process (x,continuation) ->
                let r = this.func x
                continuation r
                return! loop n
            | x -> 
                printfn "unexpected message for Worker: %A" x
                return! loop n
            }
        do! Job.start (loop 0)
        return this.workerReqCh, this.workerReplyCh
        }
 
    member this.Process(a, continuation) = this.workerReqCh *<- (Process(a,continuation))
    member this.Die() = this.workerReqCh *<+=>- Die :> Job<_>
    
type DataManager<'d,'r> =
    val readers : array<Reader<'d, 'r>>
    val dmReqCh : Ch<Msg<'d,'r>>
    val dmReplyCh : Ch<'d>

    new (r) = {readers = r; dmReqCh = Ch(); dmReplyCh = Ch()} 

    member this.Job = job {
        let dataToProcess = new System.Collections.Concurrent.ConcurrentQueue<Option<'d>>()
        let dataToFill = new System.Collections.Generic.Queue<_>()
        let dataIsEnd = ref false
        let rec loop n = job {
            let cnt = ref 3
            while dataToFill.Count > 0 && !cnt > 0 do
                decr cnt                            
                let b = dataToFill.Dequeue()
                if not <| !dataIsEnd
                then do! this.readers.[0].Read(b
                                        , fun a ->                                     
                                        dataToProcess.Enqueue a
                                        dataIsEnd := Option.isNone a)
            //if inbox.CurrentQueueLength > 0         
            //then
            let! msg = Ch.take this.dmReqCh
            match msg with
            | Die ch ->
                if !dataIsEnd 
                then
                    do! ch *<= ()
                else 
                    do! this.dmReqCh *<- Die ch
                    return! loop n
            | InitBuffers (bufs,ch) ->
                do! ch *<= bufs
                bufs |> Array.iter dataToFill.Enqueue                                
                return! loop n
            | Get(ch) -> 
                let s,r = dataToProcess.TryDequeue()
                if s
                then 
                    if r.IsNone then dataIsEnd := true
                    do! ch *<= r
                elif not !dataIsEnd
                then do! this.dmReqCh *<- Get ch
                else do! ch *<=  None
                return! loop n
            | Enq b -> 
                dataToFill.Enqueue b
                return! loop n
            | x ->  
                printfn "Unexpected message for Worker: %A" x
                return! loop n
            //else return! loop n 
        }
        do! Job.start (loop 0)
        return this.dmReqCh, this.dmReplyCh
    }

    member this.InitBuffers(bufs) = this.dmReqCh *<+=>- (fun reply -> InitBuffers(bufs, reply)) :> Job<_>
    member this.Enq(b) = this.dmReqCh *<- Enq b |> ignore
    member this.GetData(is : bool ref, w : Worker<'d, 'r>, pproc : Worker<'r,'fr> option, fw : System.Collections.Concurrent.ConcurrentQueue<Worker<'d, 'r>>) = job {
        //this.dmReqCh *<+=>- Get :> Job<_>
        do! Job.delay <| fun () ->
            let i = IVar ()
            this.dmReqCh *<+ Get i >>-. i |> ignore
            let b : Option<'d> = IVar.Now.get i
            if b.IsSome
            then w.Process(b.Value
                            ,fun a -> 
                                pproc |> Option.iter (fun p -> p.Process(a, fun _ -> ()) |> ignore)
                                fw.Enqueue w
                                this.Enq b.Value)
            else 
                is := true
                Alt.unit()          
        }
    member this.Die() = this.dmReqCh *<+=>- Die :> Job<_>


type Master<'d,'r,'fr>=        
    val workers : array<Worker<'d,'r>>
    val fill : 'd -> Option<'d>
    val bufs:ResizeArray<'d> 
    val postProcessF:Option<'r->'fr>
    val mstReqCh : Ch<Msg<'d,'r>>
    val mstReplyCh : Ch<'d>
    new (w, f, b, pproc) = {workers = w; fill = f; bufs = b; postProcessF = pproc; mstReqCh = Ch(); mstReplyCh = Ch()}    

    member this.Job = job {
        let isDataEnd = ref false
        let isEnd = ref false

        let reader = new Reader<_,_>(this.fill)
        let dataManager = new DataManager<'d,'r>([|reader|])
        let postprocessor = this.postProcessF |> Option.map(fun f ->new Worker<_,_>(f))
        let freeWorkers = new System.Collections.Concurrent.ConcurrentQueue<_>(this.workers)

        let bufers = dataManager.InitBuffers(this.bufs.ToArray())
        let rec loop n = job {
            if not freeWorkers.IsEmpty
            then 
                let success,w = freeWorkers.TryDequeue()
                if success
                then do! dataManager.GetData(isDataEnd, w, postprocessor, freeWorkers) 
            //if inbox.CurrentQueueLength > 0
            //then
            let! msg = Ch.take this.mstReqCh
            match msg with
            | Die ch ->
                do! dataManager.Die()                           
                this.workers |> Array.iter (fun w -> w.Die() |> ignore)
                match postprocessor with 
                | Some p -> 
                    do! p.Die() 
                | None -> ()
                isEnd := true
                do! ch *<= ()
            | x ->
                printfn "unexpected message for Worker: %A" x
                return! loop n 
            }
            //else return! loop n}
        do! Job.start (loop 0)
        return this.mstReqCh, this.mstReplyCh
    }

    member this.Die (c) = this.mstReqCh *<+=>- Die :> Job<_>
    member this.IsDataEnd(isDataEnd) = !isDataEnd