void (*actionGlobal) ();
void test1()
{
 void (*action) () = actionGlobal;
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
  actionGlobal = pass;
  test1 ();
  abort ();
}

x() {
  return 0;
}
