/* { dg-do compile { target nios2-*-* } } */
/* { dg-options "" } */
/* { dg-final { scan-assembler "__muldi3" } } */

long long x, y, z;

void test()
{
  x = y * z;
}
  
