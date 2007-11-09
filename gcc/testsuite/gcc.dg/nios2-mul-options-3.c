/* { dg-do compile { target nios2-*-* } } */
/* { dg-options "" } */
/* { dg-final { scan-assembler-not "__mulsi3" } } */

int x, y, z;

void test()
{
  x = y * z;
}
  
