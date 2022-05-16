(*** hide ***)
#I "../../src/Brahma.FSharp.OpenCL.Core/bin/Debug/net5.0"
#r "Brahma.FSharp.OpenCL.Core.dll"
#r "Brahma.FSharp.OpenCL.Shared.dll"

(**
# First example: arrays on GPGPU

Let's create first program which operates with arrays using GPGPU.

## Initialization

Suppose, you have .NET runtime, F# development environment, and OpenCL-compatible device.
Now you can create new F# project and add Brahma.FSharp packet to it.
*)

(**
## OpenCL device
*)

(*** hide ***)
module DeviceTest =

(**
Let's check that Brahma.fSharp can detect at least one appropriate device.
*)

    open Brahma.FSharp
    open Brahma.FSharp.OpenCL.Shared

    let main argv =
        let device = ClDevice.GetFirstAppropriateDevice()
        printfn "Device: %A" (device :> IDevice).Name

        0

(**
As a result of execution you should get a name of detected device. For example, something like this:
`
Device: "Intel(R) UHD Graphics 620 [0x5917]"
`
*)


(**
## Kernel creation
*)

(*** hide ***)
module StepByStep =

(**
Let's create `map2`-like kernel with following features.
 * Kernel should be generic. We want to create a single function for all possible types of arrays.
 * Kernel should be parameterized by operation, like a classic `map`.

It can be done with the following code.
*)

    open Brahma.FSharp
    open Brahma.FSharp.OpenCL.Shared

    let arrayMap2 operation (clContext:ClContext) workGroupSize  =
        /// The first argument of kernel function is a virtual grid. It is by contract.
        let kernel =
            <@
                fun (ndRange: Range1D) arrLength (a1:ClArray<_>) (a2:ClArray<_>) (res:ClArray<_>) ->
                    let i = ndRange.GlobalID0
                    if i < arrLength
                    then res.[i] <- (%operation) a1.[i] a2.[i]
            @>

        let kernel = clContext.Compile kernel

        /// You can inspect generated OpenCL code.
        printfn "Code: %A" kernel.Code

        /// Compilation can be done once.
        /// Compiled kernel can be executed many times on different queues with different arguments.

        fun (commandQueue:MailboxProcessor<_>) (inputArray1:ClArray<_>) (inputArray2:ClArray<_>) ->

            let ndRange =
                Range1D.CreateValid(inputArray1.Length, workGroupSize)

            /// We use default allocation mode, but it can be tuned for specific cases.
            let outputArray =
                clContext.CreateClArray(inputArray1.Length, allocationMode = AllocationMode.Default)

            /// We should use new executable object for safety.
            let kernel = kernel.GetKernel()

            commandQueue.Post(
                Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange inputArray1.Length inputArray1 inputArray2 outputArray)
            )

            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)

            outputArray

(**
Note, that our function operates over `ClArray<_>`. It allows one to provide fine-grained memory management.

## Kernel execution

To execute or kernel we should do following steps.
* Create OpenCL-compatible device.
* Create execution context. Multiple contexts can be created on the top of single device. Moreover, one can use translators with different options in different contexts.
* Get at least one queue. Queue is a way to communicate with device. Moreover, it is a synchronization primitive: commands in queue will be executed in order.
* Allocate input data.
* Create instances of kernel.
* Run instances.
* Get result back to the host.

It can be done using the following code.

*)

    open Brahma.FSharp.OpenCL.Translator

    let main (argv:array<string>) =
        // Size of arrays
        let n =
            if argv.Length > 0
            then int argv.[0]
            else 10

        let device = ClDevice.GetFirstAppropriateDevice()
        printfn "Device: %A" (device :> IDevice).Name

        let context = ClContext(device, FSQuotationToOpenCLTranslator device)

        let mainQueue = context.QueueProvider.CreateQueue()

        /// Specific instances of kernel.
        let intArraySum = arrayMap2 <@(+)@> context 32
        let boolArraySum = arrayMap2 <@(&&)@> context 32
        let arrayMask = arrayMap2 <@fun x y -> if y then x else 0@> context 32

        /// Helpers for random data generation.
        let rnd = System.Random()

        let randomIntArray () = Array.init n (fun _ -> rnd.Next() / 10000)
        let randomBoolArray () = Array.init n (fun _ -> rnd.Next() % 2 = 1)

        let intA1 = randomIntArray ()
        let intA2 = randomIntArray ()

        let boolA1 = randomBoolArray ()
        let boolA2 = randomBoolArray ()
        let boolA3 = randomBoolArray ()

        /// Allocation of input data.
        /// Data will be copied to device automatically.

        let clIntA1 = context.CreateClArray<_>(intA1)
        let clIntA2 = context.CreateClArray<_>(intA2)
        let clBoolA1 = context.CreateClArray<_>(boolA1)
        let clBoolA2 = context.CreateClArray<_>(boolA2)
        let clBoolA3 = context.CreateClArray<_>(boolA3)

        /// Evaluation.
        let intRes = intArraySum mainQueue clIntA1 clIntA2

        /// Result of first call will be passed as to the next call without copying.
        let boolRes =
            boolArraySum mainQueue clBoolA1 clBoolA2
            |> boolArraySum mainQueue clBoolA3

        let res = arrayMask mainQueue intRes boolRes

        /// Getting result as a F# array.
        /// We use PostAdReply for synchronization.
        let resOnHost = Array.zeroCreate n
        let res = mainQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(res, resOnHost, ch))

        0

(**
## Final version
*)

(*** hide ***)
module FinalVersion =

(**
Finally, full code looks like follows.
*)

    open Brahma.FSharp
    open Brahma.FSharp.OpenCL.Shared
    open Brahma.FSharp.OpenCL.Translator

    let arrayMap2 operation (clContext:ClContext) workGroupSize  =
        let kernel =
            <@
                fun (ndRange: Range1D) arrLength (a1:ClArray<_>) (a2:ClArray<_>) (res:ClArray<_>) ->
                    let i = ndRange.GlobalID0
                    if i < arrLength
                    then res.[i] <- (%operation) a1.[i] a2.[i]
            @>

        let kernel = clContext.Compile kernel

        printfn "Code: %A" kernel.Code

        fun (commandQueue:MailboxProcessor<_>) (inputArray1:ClArray<_>) (inputArray2:ClArray<_>) ->
            let ndRange =
                Range1D.CreateValid(inputArray1.Length, workGroupSize)

            let outputArray =
                clContext.CreateClArray(inputArray1.Length, allocationMode = AllocationMode.Default)

            let kernel = kernel.GetKernel()
            commandQueue.Post(
                Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange inputArray1.Length inputArray1 inputArray2 outputArray)
            )

            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)

            outputArray

    [<EntryPoint>]
    let main argv =
        let n =
            if argv.Length > 0
            then int argv.[0]
            else 10

        let device = ClDevice.GetFirstAppropriateDevice()
        printfn "Device: %A" (device :> IDevice).Name

        let context = ClContext(device, FSQuotationToOpenCLTranslator device)

        let mainQueue = context.QueueProvider.CreateQueue()

        let intArraySum = arrayMap2 <@(+)@> context 32
        let boolArraySum = arrayMap2 <@(&&)@> context 32
        let arrayMask = arrayMap2 <@fun x y -> if y then x else 0@> context 32

        let rnd = System.Random()

        let randomIntArray () = Array.init n (fun _ -> rnd.Next() / 10000)
        let randomBoolArray () = Array.init n (fun _ -> rnd.Next() % 2 = 1)

        let intA1 = randomIntArray ()
        let intA2 = randomIntArray ()

        let boolA1 = randomBoolArray ()
        let boolA2 = randomBoolArray ()
        let boolA3 = randomBoolArray ()

        let clIntA1 = context.CreateClArray<_>(intA1)
        let clIntA2 = context.CreateClArray<_>(intA2)
        let clBoolA1 = context.CreateClArray<_>(boolA1)
        let clBoolA2 = context.CreateClArray<_>(boolA2)
        let clBoolA3 = context.CreateClArray<_>(boolA3)

        let intRes = intArraySum mainQueue clIntA1 clIntA2
        let boolRes =
            boolArraySum mainQueue clBoolA1 clBoolA2
            |> boolArraySum mainQueue clBoolA3
        let res = arrayMask mainQueue intRes boolRes

        let resOnHost = Array.zeroCreate n

        let res = mainQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(res, resOnHost, ch))

        printfn "First int array:  %A" intA1
        printfn "Second int array: %A" intA2

        printfn "First bool array:  %A" boolA1
        printfn "Second bool array: %A" boolA2
        printfn "Third bool array:  %A" boolA3

        printfn "Result: %A" res

        0
