/* Verify that constants in memory, referenced only by dead code,
   are not emitted to the object file.
   FIXME: Not presently possible to apply -pedantic to code with
   complex constants in it.  The __extension__ should shut up the
   warning but doesn't.  (Hard to fix -- the lexer is not aware of
   the parser's state.)  */

/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */
/* This test fails on all processors where we use a block move to
   initialize "S" in test2.  The RTL optimizers are not clever enough
   to eliminate the block move, so the constant gets emitted.
   Currently known targets with this problem: all ARM; PA32 ("hppa*.*"
   matches "hppa2.0w" but not "hppa64"); PPC if string instructions
   are enabled (notably under AIX); S/390; Xtensa. Nios II  */
/* { dg-final { scan-assembler-not "L\\\$?C\[^A-Z\]" { xfail arm-*-* strongarm-*-* xscale-*-* nios2-*-* hppa-*-* hppa*.*-*-* powerpc*-*-aix* s390*-*-* xtensa-*-* } } } */

#define I (__extension__ 1.0iF)

struct S { int a; double b[2]; void *c; };

extern void use_str(const char *);
extern void use_S(const struct S *);
extern void use_cplx(__complex__ double);

static inline int
returns_23(void) { return 23; }

void
test1(void)
{
	if (returns_23() == 23)
		return;

	use_str("waltz, nymph, for quick jigs vex bud");
	use_S(&(const struct S){12, {3.1415, 2.1828}, 0 });
	use_cplx(3.1415 + 2.1828*I);
}

void
test2(void)
{
	const char *str = "pack my box with five dozen liquor jugs";
	const struct S S = { 23, { 1.414, 1.618 }, 0 };
	const __complex__ double cplx = 1.414 + 1.618*I;

	if (returns_23() == 23)
		return;

	use_str(str);
	use_S(&S);
	use_cplx(cplx);
}

