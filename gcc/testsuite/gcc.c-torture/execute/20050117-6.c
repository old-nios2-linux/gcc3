extern int pass ();

int test1()
{
 int (*action) () = pass;
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
  test1 ();
  abort ();
}

x() {
  return 0;
}
