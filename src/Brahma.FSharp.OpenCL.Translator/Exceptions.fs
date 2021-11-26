namespace Brahma.FSharp.OpenCL.Translator

/// The exception that is thrown when the kernel has invalid format.
exception InvalidKernelException of string

/// The exception that is thrown when the unexpected error occured during the translation.
exception TranslationFailedException of string
