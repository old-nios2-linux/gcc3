/* this used to not link on nios2 because wombat 
   would be thought to be in small data and therefore
   could be accessed as relative to gp. */

extern int wombat __attribute__((weak));

void function(void)
{
  if (&wombat != 0)
  {
    abort();
  }
}


int main()
{
  function();
  return 0;
}
