__kernel void brahmaKernel
(private int firstSide, private int secondSide, private int sumOfSides,
 __global int * firstRowsBuffer, __global int * firstColumnsBuffer, __global int * firstValuesBuffer,
 __global int * secondRowsBuffer, __global int * secondColumnsBuffer,
 __global int * secondValuesBuffer, __global int * allRowsBuffer, __global int * allColumnsBuffer,
 __global int * allValuesBuffer)
{int i = get_global_id (0) ;
 __local int beginIdxLocal ;
 __local int endIdxLocal ;
 int localID = get_local_id (0) ;
 if ((localID < 2))
 {int x = (((localID * (256 - 1)) + i) - 1) ;
  if ((x >= sumOfSides))
  {x = (sumOfSides - 1) ;} ;
  int diagonalNumber = x ;
  int leftEdge = ((diagonalNumber + 1) - secondSide) ;
  if ((leftEdge < 0))
  {leftEdge = 0 ;} ;
  int rightEdge = (firstSide - 1) ;
  if ((rightEdge > diagonalNumber))
  {rightEdge = diagonalNumber ;} ;
  while ((leftEdge <= rightEdge))
  {int middleIdx = ((leftEdge + rightEdge) / 2) ;
   ulong firstIndex
   = (((ulong) firstRowsBuffer [middleIdx] << 32) | (ulong) firstColumnsBuffer [middleIdx]) ;
   ulong secondIndex
   =
   (((ulong) secondRowsBuffer [(diagonalNumber - middleIdx)] << 32) |
    (ulong) secondColumnsBuffer [(diagonalNumber - middleIdx)]) ;
   if ((firstIndex < secondIndex))
   {leftEdge = (middleIdx + 1) ;}
   else
   {rightEdge = (middleIdx - 1) ;} ;} ;
  if ((localID == 0))
  {beginIdxLocal = leftEdge ;}
  else
  {endIdxLocal = leftEdge ;} ;} ;
 barrier(CLK_LOCAL_MEM_FENCE) ;
 int beginIdx = beginIdxLocal ;
 int endIdx = endIdxLocal ;
 int firstLocalLength = (endIdx - beginIdx) ;
 int x1 = (256 - firstLocalLength) ;
 if ((endIdx == firstSide))
 {x1 = (((secondSide - i) + localID) + beginIdx) ;} ;
 int secondLocalLength = x1 ;
 __local ulong localIndices [ 256 ] ;
 if ((localID < firstLocalLength))
 {localIndices [localID] =
  (((ulong) firstRowsBuffer [(beginIdx + localID)] << 32) |
   (ulong) firstColumnsBuffer [(beginIdx + localID)]) ;} ;
 if ((localID < secondLocalLength))
 {localIndices [(firstLocalLength + localID)] =
  (((ulong) secondRowsBuffer [(i - beginIdx)] << 32) | (ulong) secondColumnsBuffer [(i - beginIdx)])
  ;} ;
 barrier(CLK_LOCAL_MEM_FENCE) ;
 if ((i < sumOfSides))
 {int leftEdge2 = ((localID + 1) - secondLocalLength) ;
  if ((leftEdge2 < 0))
  {leftEdge2 = 0 ;} ;
  int rightEdge3 = (firstLocalLength - 1) ;
  if ((rightEdge3 > localID))
  {rightEdge3 = localID ;} ;
  while ((leftEdge2 <= rightEdge3))
  {int middleIdx4 = ((leftEdge2 + rightEdge3) / 2) ;
   ulong firstIndex5 = localIndices [middleIdx4] ;
   ulong secondIndex6 = localIndices [((firstLocalLength + localID) - middleIdx4)] ;
   if ((firstIndex5 < secondIndex6))
   {leftEdge2 = (middleIdx4 + 1) ;}
   else
   {rightEdge3 = (middleIdx4 - 1) ;} ;} ;
  int boundaryX = rightEdge3 ;
  int boundaryY = (localID - leftEdge2) ;
  uchar isValidX = (boundaryX >= 0) ;
  uchar isValidY = (boundaryY >= 0) ;
  ulong fstIdx = 0 ;
  if (isValidX)
  {fstIdx = localIndices [boundaryX] ;} ;
  ulong sndIdx = 0 ;
  if (isValidY)
  {sndIdx = localIndices [(firstLocalLength + boundaryY)] ;} ;
  if (((! isValidX) || (isValidY && (fstIdx < sndIdx))))
  {allRowsBuffer [i] = (int) (sndIdx >> 32) ;
   allColumnsBuffer [i] = (int) sndIdx ;
   allValuesBuffer [i] = secondValuesBuffer [(((i - localID) - beginIdx) + boundaryY)] ;}
  else
  {allRowsBuffer [i] = (int) (fstIdx >> 32) ;
   allColumnsBuffer [i] = (int) fstIdx ;
   allValuesBuffer [i] = firstValuesBuffer [(beginIdx + boundaryX)] ;} ;} ;}
