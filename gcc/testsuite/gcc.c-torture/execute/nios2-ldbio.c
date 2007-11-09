
volatile int x;
volatile int y;

int
main ()
{
  volatile void *p = &x;
  x = 0x1234abcd;
  y = __builtin_ldbio (p);
  if (y != 0xffffffcd)
    abort ();
  x = 0x1234000d;
  y = __builtin_ldbio (p);
  if (y != 0xd)
    abort ();
  
  return 0;
}
