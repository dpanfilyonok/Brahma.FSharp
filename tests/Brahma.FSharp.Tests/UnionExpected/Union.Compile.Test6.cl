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
{TranslateMatchTestUnion t = { 0 } ;
 int x = 5 ;
 if (((t) . tag == 1))
 {x = 6 ;}
 else
 {if (((t) . tag == 2))
  {x = 7 ;}
  else
  {x = 5 ;} ;} ;}
