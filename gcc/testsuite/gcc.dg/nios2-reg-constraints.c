/* { dg-do compile { target nios2-*-* } } */
/* { dg-final { scan-assembler "screwy r2 r3 r4 r5 r6 r7" } } */

int a, b, c, x, y, z;

void test()
{
  asm ("#screwy %0 %1 %2 %3 %4 %5" : "=D02" (a), "=D03" (b), "=D04" (c) : "D05" (x), "D06" (y), "D07" (z));
}
  
