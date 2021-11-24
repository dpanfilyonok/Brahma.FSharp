typedef struct TranslateMatchTestUnion {int tag ;
                                        union TranslateMatchTestUnion_Data {struct Case2Type {int
                                                                                              Item ;}
                                                                            Case2 ;
                                                                            struct Case3Type {int
                                                                                              Item1
                                                                                              ;
                                                                                              int
                                                                                              Item2
                                                                                              ;}
                                                                            Case3 ;} data ;} TranslateMatchTestUnion
                                                                                             ;
__kernel void brahmaKernel ()
{TranslateMatchTestUnion t = (TranslateMatchTestUnion) { 0 } ;
 int m = 5 ;
 if (((t) . tag == 1))
 {int x = (((t) . data) . Case2) . Item ;
  m = x ;}
 else
 {if (((t) . tag == 2))
  {int z = (((t) . data) . Case3) . Item2 ;
   int y = (((t) . data) . Case3) . Item1 ;
   m = (y + z) ;}
  else
  {m = 5 ;} ;} ;}
