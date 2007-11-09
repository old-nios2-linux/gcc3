/* { dg-do compile { target nios2-*-* } } */
/* { dg-options " " } */
/* { dg-final { scan-assembler-not "wrctl\\tctl6, zero" } } */

void foo(void){
  __builtin_wrctl(6,4);
}
