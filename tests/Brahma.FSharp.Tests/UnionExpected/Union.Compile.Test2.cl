#pragma OPENCL EXTENSION cl_khr_fp64 : enable
typedef struct TranslateTest {int tag ;
                              union TranslateTest_Data {struct AType {int Item1 ;
                                                                      double Item2 ;} A ;
                                                        struct BType {double Item ;} B ;} data ;} TranslateTest
                                                                                                  ;
__kernel void brahmaKernel ()
{TranslateTest x = { 1, { .B = { 5 } } } ;
 int y = 5 ;
 y = 7 ;}
