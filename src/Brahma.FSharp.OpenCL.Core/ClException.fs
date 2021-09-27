namespace Brahma.FSharp.OpenCL

open System
open OpenCL.Net
open System.Runtime.Serialization

[<Serializable>]
type CLException =
    inherit Exception

    new (error: ErrorCode) = { inherit Exception(error.ToString()) }
    
    new (error: ErrorCode, inner: Exception) = { inherit Exception(error.ToString(), inner) }
    
    new (info: SerializationInfo, context: StreamingContext) = { inherit Exception(info, context) }