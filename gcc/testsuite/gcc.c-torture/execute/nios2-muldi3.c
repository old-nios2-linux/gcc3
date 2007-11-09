#include <stdio.h>


long long __muldi3 (long long, long long);

volatile long long x;
volatile long long y;
volatile long long result;
volatile long long builtin_result;


void
mul ()
{
  result = x * y;
  builtin_result = __muldi3(x, y);
  if (result != builtin_result)
    {
      printf ("Failed x=%lld, y=%lld, x*y=%lld, __muldi3 (z, y)=%lld\n", x, y, result, builtin_result);
      abort ();
    }
}


int
main ()
{
  x = 0; y = 0; mul();
  x = -1; y = -1; mul();
  x = 1; y = 1; mul();
  x = 10; y = 10000; mul();
  x = -0x123412341234ll; y = 0x123412341234ll; mul();
  x = 0xffffffffffffffffll; y = 0xffffffffffffffffll; mul();
  x = 0x8000000000000000ll; y = 0x8000000000000000ll; mul();
  x = 0x7fffffffffffffffll; y = 0x7fffffffffffffffll; mul();
  x = 0x8000000000000000ll; y = 0x7fffffffffffffffll; mul();
  x = 0x7fffffffffffffffll; y = 0x8000000000000000ll; mul();
  x = 0x8000000000000000ll; y = 1; mul();
  x = 0x7fffffffffffffffll; y = 1; mul();
  x = 0x8000000000000000ll; y = 0; mul();
  x = 0x7fffffffffffffffll; y = 0; mul();
  x = 0x8000000000000000ll; y = 2; mul();
  x = 0x7fffffffffffffffll; y = 2; mul();
  x = 0x8000000000000000ll; y = 10; mul();
  x = 0x7fffffffffffffffll; y = 10; mul();
  x = 0x8000000000000000ll; y = 0xffffffffll; mul();
  x = 0x8000000000000000ll; y = 0xffffffff00000000ll; mul();
  x = 0x8000000000000000ll; y = 0x7fffffffll; mul();
  x = 0x8000000000000000ll; y = 0x7fffffff00000000ll; mul();
  
  return 0;
}
