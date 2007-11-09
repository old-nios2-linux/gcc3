#ifndef STACK_LIMIT
#define STACK_LIMIT "40"
#endif

/*
 * STACK_LIMIT can be redefined from the command line with
 * for example -DSTACK_LIMIT='"30"'.
 */

void test(void){
  char a[200];
}

/* 
 *int main(void)
 *{
 * test();
 * return 0;
 *}
*/
asm(
    ".global main\n"
    ".type\tmain, @function\n"
    "main:\n"
    "addi\tsp, sp, -8\n"
    /* set up et */
    "mov\tet, sp\n"
    "addi\tet, et, -" STACK_LIMIT "\n"
    /* call foo and then return. 
       may overflow depending on STACK_LIMIT */
    "stw\tra, 4(sp)\n"
    "stw\tfp, 0(sp)\n"
    "mov\tfp, sp\n"
    /* hack to get around iss return code bug */
    "movi\tr2, 55\n"
    "call\ttest\n"
    "mov\tr2, zero\n"
    "ldw\tra, 4(sp)\n"
    "ldw\tfp, 0(sp)\n"
    "addi\tsp, sp, 8\n"
    "ret\n"
    );
