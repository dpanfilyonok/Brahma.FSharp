namespace Brahma.FSharp.OpenCL

type INDRangeDimension =
    abstract member GlobalWorkSize: array<System.IntPtr> with get
    abstract member LocalWorkSize: array<System.IntPtr> with get
    abstract member Dimensions: int
    

[<Struct>]    
type _1D = 

    val globalWorkSize : int
    val localWorkSize : int
    new (_globalWorkSize, _localWorkSize) = {globalWorkSize = _globalWorkSize; localWorkSize = _localWorkSize}
    new (_globalWorkSize) = {globalWorkSize = _globalWorkSize; localWorkSize = 1}

    
    member this.GlobalID0 = 
        failwith "GPU only"
        0

    member this.LocalID0 = 
        failwith "GPU only"
        0   
        
    interface INDRangeDimension with 
    
        member this.GlobalWorkSize with 
            get () = [|System.IntPtr(this.globalWorkSize)|]

        member this.LocalWorkSize with 
            get () = [|System.IntPtr(this.localWorkSize)|]

        member this.Dimensions = 1


[<Struct>]    
type _2D =

    val globalWorkSizeX : int
    val globalWorkSizeY : int
    val localWorkSizeX : int
    val localWorkSizeY : int
    new (_globalWorkSizeX, _globalWorkSizeY, _localWorkSizeX, _localWorkSizeY) = 
        { 
            globalWorkSizeX = _globalWorkSizeX
            globalWorkSizeY = _globalWorkSizeY
            localWorkSizeX = _localWorkSizeX
            localWorkSizeY = _localWorkSizeY
        }
    
    new (_globalWorkSizeX, _globalWorkSizeY) = 
        { 
            globalWorkSizeX = _globalWorkSizeX
            globalWorkSizeY = _globalWorkSizeY
            localWorkSizeX = 1
            localWorkSizeY = 1
        }

    
    member this.GlobalID0 = 
        failwith "GPU only"
        0

    member this.GlobalID1 = 
        failwith "GPU only"
        0
      
    member this.LocalID0 = 
        failwith "GPU only"
        0

     member this.LocalID1 = 
        failwith "GPU only"
        0

    interface INDRangeDimension with 
    
        member this.GlobalWorkSize with 
            get () = [|System.IntPtr(this.globalWorkSizeX); System.IntPtr(this.globalWorkSizeY)|]

        member this.LocalWorkSize with 
            get () = [|System.IntPtr(this.localWorkSizeX); System.IntPtr(this.localWorkSizeY)|]
        
        member this.Dimensions = 2


[<Struct>]    
type _3D =

    val globalWorkSizeX : int
    val globalWorkSizeY : int
    val globalWorkSizeZ : int
    val localWorkSizeX : int
    val localWorkSizeY : int
    val localWorkSizeZ : int
    new (_globalWorkSizeX, _globalWorkSizeY, _globalWorkSizeZ, _localWorkSizeX, _localWorkSizeY, _localWorkSizeZ) = 
        { 
            globalWorkSizeX = _globalWorkSizeX
            globalWorkSizeY = _globalWorkSizeY
            globalWorkSizeZ = _globalWorkSizeZ
            localWorkSizeX = _localWorkSizeX
            localWorkSizeY = _localWorkSizeY
            localWorkSizeZ = _localWorkSizeZ
        }
    
    new (_globalWorkSizeX, _globalWorkSizeY, _globalWorkSizeZ) = 
        { 
            globalWorkSizeX = _globalWorkSizeX
            globalWorkSizeY = _globalWorkSizeY
            globalWorkSizeZ = _globalWorkSizeZ
            localWorkSizeX = 1
            localWorkSizeY = 1
            localWorkSizeZ = 1
        }

    
    member this.GlobalID0 = 
        failwith "GPU only"
        0

    member this.GlobalID1 = 
        failwith "GPU only"
        0

    member this.GlobalID2 = 
        failwith "GPU only"
        0
    
    member this.LocalID0 = 
        failwith "GPU only"
        0
    
    member this.LocalID1 = 
        failwith "GPU only"
        0

    member this.LocalID2 = 
        failwith "GPU only"
        0
    
    interface INDRangeDimension with 
    
        member this.GlobalWorkSize with 
            get () = [|System.IntPtr(this.globalWorkSizeX); System.IntPtr(this.globalWorkSizeY); System.IntPtr(this.globalWorkSizeZ)|]

        member this.LocalWorkSize with 
            get () = [|System.IntPtr(this.localWorkSizeX); System.IntPtr(this.localWorkSizeY); System.IntPtr(this.localWorkSizeZ)|]

        member this.Dimensions = 3