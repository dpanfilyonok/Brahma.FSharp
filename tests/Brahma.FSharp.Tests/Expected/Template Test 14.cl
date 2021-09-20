int g (private int y2, private int r, private int t)
{return ((r + y2) - t) ;}
int n (private int y2, private int o)
{return (o - g (y2, y2, 2)) ;}
int g3 (private int y2, private int m)
{return n (y2, 5) ;}
int f (private int y)
{int y1 = y ;
 int y2 = y1 ;
 return g3 (y2, y2) ;}
int z (private int y4)
{return (y4 - 2) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = f (z (7)) ;}
