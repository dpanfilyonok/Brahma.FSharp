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
    | Die //of IVar<unit>
    | Process of 'data*('res -> unit)
    | PostProcess of 'res
    | Fill of 'data*(Option<'data> -> unit)
    | InitBuffers of array<'data>*IVar<array<'data>>
    | Get of IVar<Option<'data>>
    | Enq of 'data

type Reader<'d>  (fillF:'d -> Option<'d>) = 

    member this.Read inCh (a, cont) = 
        printfn "%s" "read"
        inCh *<+ (Fill(a, cont))     
    //member this.Die (ch : Ch<_>) = run <| job {
        //printfn "%s" "r must die"
        //let reply = IVar()
        //ch *<+ Die reply >>-. reply |> ignore
        //}
        //inCh *<+=>- Die :> Job<_> // здесь и подобных были таймауты... могут понадобиться
    
    member this.Create : Job<Ch<Msg<'d,_>>>= job {
        let inCh = Ch()
        let rec loop n = job {
            printfn "%s" "reader exists"
            let! msg = Ch.take inCh
            match msg with
            | Die -> 
                printfn "%s" "reader msg1"
                //do! ch *<= ()
                do! Job.abort() 
            | Fill (x, cont) ->
                printfn "%s" "reader msg2"
                let filled = fillF x
                cont filled
                if filled.IsNone
                then do! inCh *<+ Die //this.Die(inCh) 
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
            | Die ->
                printfn "%s" "worker msg1"
                //do! ch *<= ()
                do! Job.abort() 
            | Process (x,continuation) ->
                printfn "%s" "worker msg2"
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
 
    member this.Process inCh (a, continuation) = 
        printfn "%s" "process"
        inCh *<+ (Process(a,continuation))
    //member this.Die inCh = 
        //printfn "%s" "w must die"
        //inCh *<+=>- Die :> Job<_>
 
    
type DataManager<'d>(reader:Reader<'d>) =

    member this.Create = job {
        let inCh = Ch()
        let! rCh = reader.Create   
        let dataToProcess = new System.Collections.Concurrent.ConcurrentQueue<Option<'d>>()
        let dataToFill = new System.Collections.Generic.Queue<_>()
        let dataIsEnd = ref false
        let f a = dataToProcess.Enqueue a
                  dataIsEnd := Option.isNone a
        let rec loop n = job {
            //let cnt = ref 3
            while dataToFill.Count > 0 (*&& !cnt > 0*) do
                //decr cnt                            
                let b = dataToFill.Dequeue()
                if not <| !dataIsEnd
                then do! reader.Read rCh (b, fun a -> f a)
            let! msg = Ch.take inCh
            match msg with
            | Die ->
                printfn "%s" "dm msg1"
                if !dataIsEnd 
                then 
                    //do! ch *<= ()
                    do! Job.abort() 
                else 
                    do! inCh *<+ Die
                    return! loop n
            | InitBuffers (bufs,ch) ->
                printfn "%s" "dm msg2"
                do! ch *<= bufs
                bufs |> Array.iter dataToFill.Enqueue                                
                return! loop n
            | Get ch -> 
                printfn "%s" "dm msg3"
                let s,r = dataToProcess.TryDequeue()
                if s
                then 
                    if r.IsNone 
                    then
                        printfn "None!"
                        dataIsEnd := true
                    do! ch *<= r    
                elif not !dataIsEnd
                then do! inCh *<+ Get ch        
                else do! ch *<=  None                    
                return! loop n
            | Enq b -> 
                printfn "%s" "dm msg4"
                dataToFill.Enqueue b
                return! loop n
            | x ->  
                printfn "Unexpected message for Worker: %A" x
                return! loop n
        }
        do! Job.start (loop 0)      
        return inCh
    }

    member this.InitBuffers ch bufs = Job.delay <| fun () ->
        printfn "%s" "bufs"
        let reply = IVar()
        ch *<+ InitBuffers(bufs, reply) >>-. reply
        //inCh *<+=>- (fun reply -> InitBuffers(bufs, reply)) :> Job<_>
    member this.Enq inCh b = 
        printfn "%s" "enq"
        inCh *<+ Enq b
    member this.GetData inCh = inCh *<+=>- (fun reply -> Get reply) :> Job<_> 
    //member this.Die inCh = 
        //printfn "%s" "dm must die"
        //inCh *<+=>- Die :> Job<_>


type Master<'d,'r,'fr>(workers:array<Worker<'d,'r>>, fill: 'd -> Option<'d>, bufs:ResizeArray<'d>, postProcessF:Option<'r->'fr>) =             
    
    let isDataEnd = ref false
    member this.Create = job {  
        let isEnd = ref false
        let reader = new Reader<'d>(fill)     
        let dataManager = new DataManager<'d>(reader)
        let postprocessor = postProcessF |> Option.map(fun f -> new Worker<_,_>(f))
        let freeWorkers = new System.Collections.Concurrent.ConcurrentQueue<_>(workers)  
        let inCh = Ch()
        let! dmCh = dataManager.Create
        let! bufers = dataManager.InitBuffers dmCh (bufs.ToArray())
        let rec loop n = job {       
            if not freeWorkers.IsEmpty
            then 
                let success,w = freeWorkers.TryDequeue()
                if success
                then
                    let! b = dataManager.GetData dmCh
                    if b.IsSome
                    then
                        let! wCh = w.Create
                        if postprocessor.IsSome 
                        then
                            let! ppCh = postprocessor.Value.Create
                            do! w.Process wCh
                                    (b.Value
                                    , fun a -> run <| job {
                                          do! postprocessor.Value.Process ppCh (a, fun _ -> ()) 
                                          freeWorkers.Enqueue w
                                          do! dataManager.Enq dmCh b.Value}) 
                        else 
                            do! w.Process wCh
                                    (b.Value
                                    , fun a -> run <| job {
                                          freeWorkers.Enqueue w
                                          do! dataManager.Enq dmCh b.Value})
                    else 
                        isDataEnd := true 
            let! msg = Ch.take inCh
            match msg with
            | Die ->
                printfn "%s" "master msg1"
                //do! dataManager.Die dmCh 
                do! dmCh *<+ Die
                for i = 0 to workers.Length - 1 do
                    let! iwCh = workers.[i].Create                       
                    do! iwCh *<+ Die //workers.[i].Die iwCh 
                if postprocessor.IsSome 
                then
                    let! ppCh = postprocessor.Value.Create
                    printfn "%s" "ppCh was create again" 
                    do! ppCh *<+ Die //postprocessor.Value.Die ppCh 
                isEnd := true
                //do! ch *<= ()
                do! Job.abort() 
            | x ->
                printfn "unexpected message for Worker: %A" x
                return! loop n 
            }
        do! Job.start (loop 0)
        return inCh
    }

    member this.IsDataEnd = !isDataEnd
    //member this.Die inCh c = 
        //printfn "%s" "mst must die"
        //inCh *<+=>- Die :> Job<_>