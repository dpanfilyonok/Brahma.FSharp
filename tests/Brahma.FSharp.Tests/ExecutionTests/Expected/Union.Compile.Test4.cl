typedef struct SimpleUnion {int tag ;
                            union SimpleUnion_Data {struct SimpleTwoType {int Item ;} SimpleTwo ;}
                            data ;} SimpleUnion ;
typedef struct OuterUnion {int tag ;
                           union OuterUnion_Data {struct OuterType {int Item ;} Outer ;
                                                  struct InnerType {SimpleUnion Item ;} Inner ;}
                           data ;} OuterUnion ;
__kernel void brahmaKernel ()
{OuterUnion x = { 1, { .Inner = { { 0 } } } } ;
 int y = 5 ;
 y = 7 ;}
