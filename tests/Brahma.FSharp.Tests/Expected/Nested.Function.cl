int f (private int x, private int y)
{return (x - y) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = f (2, 3) ;
 buf [1] = f (4, 5) ;}
