#pragma OPENCL EXTENSION cl_khr_fp64 : enable
double ItemUnitFunc (__global double * buf, private double tempVarY)
{double tempVarX = buf [0] ;
 double tempVarY1 = tempVarY ;
 if ((tempVarX > tempVarY1))
 {return tempVarX ;}
 else
 {return tempVarY1 ;} ;}
double ItemUnitFunc2 (__global double * buf, private double tempVarY)
{double tempVarX2 = buf [0] ;
 double tempVarY3 = tempVarY ;
 if ((tempVarX2 > tempVarY3))
 {return tempVarX2 ;}
 else
 {return tempVarY3 ;} ;}
__kernel void brahmaKernel (__global double * buf)
{double tempVarY = 1 ;
 buf [0] = ItemUnitFunc (buf, tempVarY) ;
 buf [0] = ItemUnitFunc2 (buf, tempVarY) ;}
