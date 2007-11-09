/* { dg-do compile { target nios2-*-* } } */
/* { dg-options " " } */
/* { dg-final { scan-assembler-not "bgeu\\tsp, et" } } */
/* { dg-final { scan-assembler-not "break\\t3" } } */
/* check stack checking */
void test()
{
  int a, b, c;
}
