__kernel void brahmaKernel (__global int * buf)
{int i = 1 ;
 for (int i1 = 0 ; (i1 <= (i + 1)) ; i1 ++)
 {int i2 = (i1 + 2) ;
  buf [i2] = 2 ;} ;}
