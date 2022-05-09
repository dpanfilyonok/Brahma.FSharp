# Brahma.FSharp 

[![FAKE Build](https://github.com/YaccConstructor/Brahma.FSharp/actions/workflows/build-on-push.yml/badge.svg)](https://github.com/YaccConstructor/Brahma.FSharp/actions/workflows/build-on-push.yml) 
[![NuGet Badge](https://buildstats.info/nuget/Brahma.FSharp)](https://www.nuget.org/packages/Brahma.FSharp/)
[![NuGet Badge](https://buildstats.info/nuget/Brahma.FSharp?includePreReleases=true)](https://www.nuget.org/packages/Brahma.FSharp/)
[![License](https://img.shields.io/badge/License-EPL_1.0-red.svg)](https://opensource.org/licenses/EPL-1.0)

**Brahma.FSharp** provides a way to utilize GPGPUs in your F# programms. It is based on F# quotations to OpenCL translation.

Features of Brahma.FSharp:
* Utilization of OpenCL for communication with GPU. So, you can work not only with NVIDIA devices but with any device which supports OpenCL (e.g. with AMD ot Intel devices).
* Not only primitive types, but olso discriminated unions, structs, records are supported.
* Pattern matching, mutable and immutable bindings, nested bindings are supported.
* Fine-grained memory managenent and kernels compilation porcess.
* Mailbox prcessor based interface for communication with devices.

More detailes are available [here](https://yaccconstructor.github.io/Brahma.FSharp/).
Examples of usage are available [here](https://github.com/YaccConstructor/Brahma.FSharp.Examples).

---

### Developing

We use [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold). 

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://dotnet.microsoft.com/en-us/download/dotnet/5.0) 5.0 or higher
- OpenCL-compatible device and respective OpenCL driver. 

---

### Build

To build and run all tests:

On Windows
```cmd
build.cmd 
```

On Linux/macOS
```sh
./build.sh 
```

To find more options look at [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold). 
