namespace Brahma.FSharp.OpenCL

open OpenCL.Net
open Brahma.OpenCL
open GraphBLAS.FSharp.Backend.COOMatrix.Utilities
open GraphBLAS.FSharp.Backend.COOMatrix

module Lib =

    let getNewMAddM<'t> (gpu: GPU) op =
        let kernelFun =
            <@
                fun (r:_2D) mSize (a:array<'t>) (b:array<'t>) (c:array<'t>) ->
                    let tx = r.GlobalID0
                    let ty = r.GlobalID1
                    c.[ty * mSize + tx] <- (%op) a.[ty * mSize + tx] b.[ty * mSize + tx]
            @>

        let kernel = gpu.CreateKernel(kernelFun)
        fun (processor:MailboxProcessor<_>) rng mSize (a:GpuArray<'t>) (b:GpuArray<'t>) (res:GpuArray<'t>) ->
            kernel.SetArguments rng mSize a b res
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

    let getNewMxMAdd<'t1,'t2,'t3> (gpu: GPU) opAdd opMult =
        let kernelFun =
            <@
                fun (r:_2D) mSize (a:array<'t1>) (b:array<'t2>) (c:array<'t3>) ->
                    let tx = r.GlobalID0
                    let ty = r.GlobalID1
                    let mutable buf = c.[ty * mSize + tx]
                    for k in 0 .. mSize - 1 do
                        buf <- (%opAdd) buf  ((%opMult) a.[ty * mSize + k]  b.[k * mSize + tx])
                    c.[ty * mSize + tx] <- buf
            @>

        let kernel = gpu.CreateKernel(kernelFun)

        fun (processor:MailboxProcessor<_>) rng mSize (a:GpuArray<'t1>) (b:GpuArray<'t2>) (res:GpuArray<'t3>) ->
            kernel.SetArguments rng mSize a b res
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

    let getNewVectorVectorElementwiseOp<'t1,'t2,'t3> (gpu:GPU) op =
        let kernelFun =
            <@ fun (range:_1D) (a1:array<'t1>) (a2:array<'t2>) (res:array<'t3>) ->
                let i = range.GlobalID0
                res.[i] <- (%op) a1.[i] a2.[i] @>
        let kernel = gpu.CreateKernel(kernelFun)
        fun (processor:MailboxProcessor<_>) rng (a1:GpuArray<'t1>) (a2:GpuArray<'t2>) (res:GpuArray<'t3>) ->
            kernel.SetArguments rng a1 a2 res
            processor.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

type Host() =

    let kernelFun =
        <@ fun (range:_1D) (buf:array<_>) n ->
            let i = range.GlobalID0
            buf.[i] <- buf.[i] * n @>

    let mxmQuadro () =

        let devices = Device.getDevices "*NVIDIA*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        let processors = Array.init 4 (fun _ -> gpu.GetNewProcessor ())

        let mSize = 3 * 1024
        let size = mSize * mSize
        let localWorkSize = 32
        let d = (new _2D(mSize, mSize, localWorkSize, localWorkSize))

        let aBlocks = Array.init 4 (fun _ -> Array.init size (fun _ -> 1))
        let bBlocks = Array.init 4 (fun _ -> Array.init size (fun _ -> 2))
        let resBlocks = Array.init 4 (fun _ -> Array.zeroCreate size)

        let _aBlocks = Array.init 4 (fun _ -> gpu.Allocate<_>(size))
        let _bBlocks = Array.init 4 (fun _ -> gpu.Allocate<_>(size))
        let _resBlocks = Array.init 4 (fun _ ->  gpu.Allocate<_>(size))

        let mxm = Array.init 4 (fun _ -> Lib.getNewMxMAdd<_,_,_> gpu <@ (+) @> <@ (*) @>)
        let mam = Array.init 4 (fun _ -> Lib.getNewMAddM<   int> gpu <@ (+) @>)

        let barrier1 = Msg.CreateBarrierMessages 4

        processors
        |> Array.iteri (fun i p ->
            p.Post(Msg.CreateToGPUMsg<_>(aBlocks.[i], _aBlocks.[i]))
            p.Post(Msg.CreateToGPUMsg<_>(bBlocks.[i], _bBlocks.[i]))
            p.Post(Msg.CreateToGPUMsg<_>(resBlocks.[i], _resBlocks.[i]))
            p.Post(barrier1.[i])
            )

        for i in 0..1 do
            for j in 0..1 do
                for k in 0..1 do
                    let mxm = mxm.[i * 2 + j]
                    let mam = mam.[i * 2 + j]
                    mxm processors.[i * 2 + j] d mSize _aBlocks.[i * 2 + k] _bBlocks.[k * 2 + j] _resBlocks.[i * 2 + j]

        let barrier2 = Msg.CreateBarrierMessages 4
        Array.iteri2
            (fun i (p:MailboxProcessor<_>) b ->
                p.Post(b)
            )
            processors
            barrier2

        processors
        |> Array.mapi (fun i p -> p.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(_resBlocks.[i], resBlocks.[i], ch))))

        resBlocks
        |> Array.iter (fun x -> printfn "%A" x)



    let f1 () =
        let devices = Device.getDevices "*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        let processor1 = gpu.GetNewProcessor ()
        let processor2 = gpu.GetNewProcessor ()
        let a1 = Array.init 10 (fun i -> i + 1)
        let a2 = Array.init 20 (fun i -> float i + 2.0)
        let res1 = Array.zeroCreate 10
        let res2 = Array.zeroCreate 20

        let n1 = gpu.Allocate<_>(a1.Length)
        let n2 = gpu.Allocate<_>(a1.Length)
        let nRes = gpu.Allocate<_>(a1.Length)

        let m1 = gpu.Allocate<_>(a2.Length)
        let m2 = gpu.Allocate<_>(a2.Length)
        let mRes = gpu.Allocate<_>(a2.Length)


        //TODO: simplify it!!!
        processor1.Post(Msg.CreateToGPUMsg<_>(a1, n1))
        processor1.Post(Msg.CreateToGPUMsg<_>(a1, n2))

        processor2.Post(Msg.CreateToGPUMsg<_>(a2, m1))
        processor2.Post(Msg.CreateToGPUMsg<_>(a2, m2))

        let vAdd = Lib.getNewVectorVectorElementwiseOp<_,_,_> gpu <@ (+) @>
        let vMult = Lib.getNewVectorVectorElementwiseOp<_,_,_> gpu <@ (*) @>

        let _1d1 = new _1D(a1.Length,1)
        let _1d2 = new _1D(a2.Length,1)

        let r1 = vAdd processor2 _1d1 n1 n2 nRes
        let r2 = vMult processor1 _1d2 m1 m2 mRes
        //kernel.SetArguments _1d m1 6

        //processor1.Post(Msg.CreateRunMsg(Run<_,_,_>(kernel)))

        //This code is unsafe because there is no synchronization between processors
        //It is just to show that we can share buffers between queues on the same device
        let res1 = processor2.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(nRes, res1, ch)))
        let res2 = processor1.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(mRes, res2, ch)))

        let result =
            res1,res2

        result

    let prefix (gpu:GPU) =

        let processor = gpu.GetNewProcessor ()

        let arr = Array.init 1000 (fun i -> 1)
        let res = Array.zeroCreate 1

        let gpuArr = gpu.Allocate<_>(1000)
        let gpuRes = gpu.Allocate<_>(1)

        processor.Post(Msg.CreateToGPUMsg<_>(arr, gpuArr))

        let sum = GraphBLAS.FSharp.Backend.Common.PrefixSum.runExcludeInplace gpu
        let _,r = sum processor gpuArr gpuRes
        processor.PostAndReply(fun ch -> Msg.CreateToHostMsg(ToHost<_>(r, res, ch)))

        printfn "Sum: %A" res

    let m_m_add (gpu:GPU) =

        let processor = gpu.GetNewProcessor ()

        let cols1 = Array.init 1000 (fun i -> i)
        let cols2 = Array.init 1000 (fun i -> i)
        let rows1 = Array.init 1000 (fun i -> i)
        let rows2 = Array.init 1000 (fun i -> i)
        let values1 = Array.init 1000 (fun i -> 1)
        let values2 = Array.init 1000 (fun i -> 1)

        let mtx1 =
            {
                RowCount = 1000
                ColumnCount = 1000
                Rows = rows1
                Columns = cols1
                Values = values1
            }

        let mtx2 =
            {
                RowCount = 1000
                ColumnCount = 1000
                Rows = rows2
                Columns = cols2
                Values = values2
            }

        let resMtx = EWiseAdd.run gpu mtx1 mtx2 <@ (+) @>

        printfn "Sum: %A" resMtx.Values


    member this.Do () =

        let devices = Device.getDevices "*" DeviceType.Gpu
        printfn "Device: %A" devices.[0]
        let gpu = GPU(devices.[0])
        m_m_add gpu

        [||],[||]

