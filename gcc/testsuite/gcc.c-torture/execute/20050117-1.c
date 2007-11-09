void
test1 (void (*action) ())
{
  x ();
  action ();
}
 
void 
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
