int f (private int x, private int y)
{int x1 = x ;
 return (x1 + y) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = f (7, 8) ;}
