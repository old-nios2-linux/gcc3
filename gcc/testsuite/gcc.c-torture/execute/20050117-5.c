int (*actionGlobal) ();
int test1()
{
 int (*action) () = actionGlobal;
 x();
 return action();
}
 
int 
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
