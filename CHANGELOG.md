# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0-alpha9] - 2021-10-15

### Added
- Workflow builder for OpenCL computations

### Fixed
- Boolean type support (issue ##116, https://github.com/YaccConstructor/Brahma.FSharp/issues/116)

## [2.0.0-alpha8] - 2021-09-27

### Added
- New mailbox processor based API
- Targeting .net 5.0

## [2.0.0-alpha7.1] - 2021-07-18

### Added
- Kernel compilation caching

## [2.0.0-alpha7] - 2021-05-19

### Fixed
- Nested functions
- Complex let bindings

### Added
- Mutable variables in closures

## [2.0.0-alpha6.2] - 2021-05-19

### Fixed
- ToHost behaviour on non-gpu arrays

## [2.0.0-alpha6.1] - 2021-03-22

### Fixed
- printf/printfn without arguments

## [2.0.0-alpha6] - 2021-03-22

### Added
- support of printf call inside kernel code

## [2.0.0-alpha5] - 2021-01-27

### Fixed
- Boolean binary operators
- Transfer arrays of boolean

## [2.0.0-alpha4] - 2020-12-27

### Added
- While and for loops in workflow builders

## [2.0.0-alpha3]

### Fixed
- Local memory semantic. It is forbidden to initialize variables in the local memory.

### Added
- Basic workflow builders for designing computations

## [2.0.0-alpha2]

- Atomic functions are polimorphic
- Function for allocation in local memory is revised
- Type provider is removed
- NetStandard 2.1 and .NET 4.6.1 are targeted

## [1.1.5]

- Constant array translation improved.

## [1.1.4]

- Fix translation of access to local ids of 2D.

## [1.1.3]

- Fix OpenCL compiller options.

## [1.1.2]

- More diagnostic information in kernel compiler

## [1.1.1]

- PDBs added
- Autoinstallation of OpenCL.Net.dll.config

## [1.1.0]

- Basic support of structs and tuples.
- OpenCL type provider. Strongly typed kernels from OpenCL code are available in F#.
- Documentation updated.
- More examples added.

## [1.1.0-alpha4]

- Basic support of structs and tuples.

## [1.1.0-alpha3]

- Clean references.

## [1.1.0-alpha2]

- Fix references.

## [1.1.0-alpha1]

- OpenCL type provider. Strongly typed kernels from OpenCL code are available in F#.

## [1.0.1]

- Fix Float type translation

## [1.0.0]

- FSharp.Core form NuGet
- .NET 4.5

[Unreleased]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.0-alpha9...HEAD
[2.0.0-alpha9]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9
[2.0.0-alpha9]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9
[2.0.0-alpha8]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha8
[2.0.0-alpha7.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha7.1
[2.0.0-alpha7]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha7
[2.0.0-alpha6.2]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha6.2
[2.0.0-alpha6.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha6.1
[2.0.0-alpha6]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha6
[2.0.0-alpha5]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha5
[2.0.0-alpha4]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha4
