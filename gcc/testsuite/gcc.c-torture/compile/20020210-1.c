/* PR c/5615 */
void f(int a, struct {int b[a];} c) {} /* { dg-bogus "Do not know how to handle large structs or variable length types" "" { xfail nios2-*-* } } */
