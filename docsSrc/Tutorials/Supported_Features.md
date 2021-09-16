# Supported Features.

Translator supports only subset of F# language and tis subset is described here.

## OpenCL specific operations
 * [Data transfer operations](Brahma.FSharp/Api_Reference/Brahma.FSharp.OpenCL.Extensions/brahma-fsharp-opencl-extensions.html)
 * [Supported kernel operations](Brahma.FSharp/Api_Reference/Brahma.FSharp.OpenCL.Extensions/global-opencl.html).
 * Supported functions from System.Math and Microsoft.FSharp.Core.Operators: abs, acos, asin, atan, cos, cosh, exp, floor, log, log10, pow, sin, sinh, sqrt, tan, tanh


## Basic constructions.

### Array access
Array "by index" access is supported.

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            buf.[1] <- buf.[0]
    @>
```

### Binding
Basic "let" binding is supported. Note, that now we support only "variable bindings". Nested functions, closures are not supported.

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            let x = 1
            let y = (x -1) * (x + 2)
    @>

```

### Mutable binding
Mutability is available by using of "let mutable" binding.

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            let mutable x = 1
            x <- x * 2
    @>
```

Note, that scopes are supported. So, you can "rebind" any name and "F#-style" visibility will be emuleted in target code. For example, next code will be translated correctly.

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            let i = 2
            for i in 1..3 do     
                buf.[i] <- buf.[i] + 1
            buf.[0] <- i
    @>

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            for i in 1..3 do
                let i = i * 2     
                buf.[i] <- 0
    @>
```

### Expression ignore by ```|> ignore```
```
let command = 
    <@
        fun (range: _1D) (buffer: int[]) ->
            let gid = range.GlobalID0
            atomic inc buffer.[gid] |> ignore
    @>
```

## Control flow

### Sequential operations

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) ->
            buf.[0] <- 2
            buf.[1] <- 4
    @>
```

### WHILE loop

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
          while buf.[0] < 5 do
            buf.[0] <- buf.[0] + 1
    @>
```

### FOR integer range loop

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) -> 
            for i in 1..3 do buf.[i] <- 0
    @>
```

### Quotations injection
You can use "quotations injection" for code reusing or parameterization. For example, you can write something like this:

```
let myFun = <@ fun x y -> y - x @>
let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) ->
            buf.[0] <- (%myFun) 2 5
            buf.[1] <- (%myFun) 4 9
    @>

let commandTemplate f = 
    <@ 
        fun (range:_1D) (buf:array<int>) ->
            buf.[0] <- (%f) 2 5
            buf.[1] <- (%f) 4 9
    @>

let cmd1 = commandTemplate  <@ fun x y -> y - x @>
let cmd2 = commandTemplate  <@ fun x y -> y + x @>
```


## Structs and tuples

Structs and tuples transferring and using in kernel code are supported.

### Structs

```
[<Struct>]
type c =
    val x: int 
    val y: int
    new (x1, y1) = {x = x1; y = y1} 
    new (x1) = {x = x1; y = 0}

let command = 
    <@ 
        fun(range:_1D) (buf:array<int>) (s:c) -> 
            buf.[0] <- s.x + s.y
            let s2 = new c(6)
            let s3 = new c(s2.y + 6)
            buf.[1] <- s2.x
    @>

let command = 
    <@ 
        fun(range:_1D) (buf:array<int>) (arr:array<c>) -> 
            buf.[0] <- arr.[0].x         
    @>
```

### Tuples

Tuples support are limited. Single tuple trunsfer to GPU is supported, but methods Array.ToHost and Array.ToGPU is not.
Also you can use tuples in kernel code.

```
let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) (k1:int*int) (k2:int64*byte) (k3:float32*int) -> 
            let x = fst k1
            buf.[0] <- x
            buf.[1] <- int(fst k3)
    @>

let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) -> 
            let (a, b) = (1, 2)
            buf.[0] <- a
    @>
```
