int
test1 (int (*action) ())
{
  x ();
  return action ();
}
 
int 
pass ()
{
  exit (0);
}

int
main ()
{
  test1 (pass);
  abort ();
}

x() {
  return 0;
}
