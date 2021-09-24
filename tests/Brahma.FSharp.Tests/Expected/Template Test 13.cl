int g (private int m)
{return (m + 1) ;}
int f (private int y)
{int y1 = y ;
 int y2 = y1 ;
 return g (y2) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = f (7) ;}
