typedef float floatvect2 __attribute__((mode(V4SF))); /* { dg-error "unable to emulate" "" { target nios2-*-* } } */

typedef union
{
    floatvect2 vector;
    float f[2];
}resfloatvect2;

void tempf(floatvect2 *x, floatvect2 *y)
{
        floatvect2 temp= *x;
        floatvect2 temp1=*y;
        resfloatvect2 temp2;
        *x=temp+temp1;
}
