/* { dg-do compile { target nios2-*-* } } */
/* { dg-options "-mstack-check" } */
/* { dg-final { scan-assembler "bgeu\\tsp, et" } } */
/* { dg-final { scan-assembler "break\\t3" } } */
/* check stack checking */
void test()
{
  int a, b, c;
}
