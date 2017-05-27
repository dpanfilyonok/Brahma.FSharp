### Restrictions:

* function declarations, preprocessor macros and single-line comments are allowed at the top level
* preprocessor macros are ignored and don't have any impact on the parsing process
* only basic data types are supported (see below in the grammar)
* unions are not supported
* anonymous structs as function parameters are not supported
* `__attribute__` specifiers for kernel functions are not supported
* `inline` function specifier is not supported in OpenCL C
* Unicode in identifiers is not supported
* type casting in function definitions is not supported

### C99 keywords:

    auto break case char const continue default do double else enum extern
    float for goto if inline int long register restrict return short signed
    sizeof static struct switch typedef union unsigned void volatile while
    _Bool _Complex _Imaginary

### OpenCL C 2.0 additional keywords:

* Data types:
        bool
        uchar ushort uint ulong
        half

* Address space qualifiers:

        __global global __local local
        __constant constant __private private
        __generic generic (* reserved *)

* Function qualifiers:

        __kernel kernel

* Access qualifiers:

        __read_only read_only
        __write_only write_only
        __read_write read_write

* Additional:

        uniform pipe

Additional reference: https://www.fixstars.com/en/opencl/book/OpenCLProgrammingBook/opencl-c/
