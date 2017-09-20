open ArrayGPU
open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions 

type BrahmaBuilder () =
  let platformName = "NVIDIA*"
  let deviceType = DeviceType.Default        
    
  let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
  let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head) 
  
  member this.Zero() =
        printfn "Zero"
        None    
   
  member this.Return (x) =
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    x

let Indus = new BrahmaBuilder()