/* { dg-do compile { target nios2-*-* } } */
/* { dg-options "-mhw-mulx" } */
/* { dg-final { scan-assembler-not "__muldi3" } } */

long long x, y, z;

void test()
{
  x = y * z;
}
  
