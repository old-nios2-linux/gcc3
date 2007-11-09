extern void pass ();

void test1()
{
 void (*action) () = pass;
 x();
 action();
}
 
void 
pass ()
{
  exit (0);
}

int
main ()
{
  test1 ();
  abort ();
}

x() {
  return 0;
}
