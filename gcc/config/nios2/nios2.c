/* Subroutines for assembler code output for Altera NIOS 2G NIOS2 version.
   Copyright (C) 2003 Altera
   Contributed by Jonah Graham (jgraham@altera.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include <stdio.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "toplev.h"
#include "basic-block.h"
#include "function.h"
#include "ggc.h"
#include "reload.h"
#include "debug.h"
#include "optabs.h"
#include "target.h"
#include "target-def.h"

/* local prototypes */
static bool nios2_rtx_costs (rtx, int, int, int *);

static void nios2_asm_function_prologue (FILE *, HOST_WIDE_INT);
static int nios2_use_dfa_pipeline_interface (void);
static int nios2_issue_rate (void);
static struct machine_function *nios2_init_machine_status (void);
static bool nios2_in_small_data_p (tree);
static rtx save_reg (int, HOST_WIDE_INT, rtx);
static rtx restore_reg (int, HOST_WIDE_INT);
static unsigned int nios2_section_type_flags (tree, const char *, int);
static void nios2_init_builtins (void);
static rtx nios2_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
static bool nios2_function_ok_for_sibcall (tree, tree);
static void nios2_encode_section_info (tree, rtx, int);

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE nios2_asm_function_prologue

#undef TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE
#define TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE \
 nios2_use_dfa_pipeline_interface
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE nios2_issue_rate
#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P nios2_in_small_data_p
#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO nios2_encode_section_info
#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS  nios2_section_type_flags

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS nios2_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN nios2_expand_builtin

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL nios2_function_ok_for_sibcall

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS nios2_rtx_costs


struct gcc_target targetm = TARGET_INITIALIZER;



/* Threshold for data being put into the small data/bss area, instead
   of the normal data area (references to the small data/bss area take
   1 instruction, and use the global pointer, references to the normal
   data area takes 2 instructions).  */
unsigned HOST_WIDE_INT nios2_section_threshold = NIOS2_DEFAULT_GVALUE;


/* Structure to be filled in by compute_frame_size with register
   save masks, and offsets for the current function.  */

struct nios2_frame_info
GTY (())
{
  long total_size;		/* # bytes that the entire frame takes up */
  long var_size;		/* # bytes that variables take up */
  long args_size;		/* # bytes that outgoing arguments take up */
  int save_reg_size;		/* # bytes needed to store gp regs */
  int save_reg_rounded;		/* # bytes needed to store gp regs */
  long save_regs_offset;	/* offset from new sp to store gp registers */
  int initialized;		/* != 0 if frame size already calculated */
  int num_regs;			/* number of gp registers saved */
};

struct machine_function
GTY (())
{

  /* Current frame information, calculated by compute_frame_size.  */
  struct nios2_frame_info frame;
};


/***************************************
 * Section encodings
 ***************************************/





/***************************************
 * Stack Layout and Calling Conventions
 ***************************************/


#define TOO_BIG_OFFSET(X) ((X) > ((1 << 15) - 1))
#define TEMP_REG_NUM 8

static void
nios2_asm_function_prologue (FILE *file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (flag_verbose_asm || flag_debug_asm)
    {
      compute_frame_size ();
      dump_frame_size (file);
    }
}

static rtx
save_reg (int regno, HOST_WIDE_INT offset, rtx cfa_store_reg)
{
  rtx insn, stack_slot;

  stack_slot = gen_rtx_PLUS (SImode,
			     cfa_store_reg,
			     GEN_INT (offset));

  insn = emit_insn (gen_rtx_SET (SImode,
				 gen_rtx_MEM (SImode, stack_slot),
				 gen_rtx_REG (SImode, regno)));

  RTX_FRAME_RELATED_P (insn) = 1;

  return insn;
}

static rtx
restore_reg (int regno, HOST_WIDE_INT offset)
{
  rtx insn, stack_slot;

  if (TOO_BIG_OFFSET (offset))
    {
      stack_slot = gen_rtx_REG (SImode, TEMP_REG_NUM);
      insn = emit_insn (gen_rtx_SET (SImode,
				     stack_slot,
				     GEN_INT (offset)));

      insn = emit_insn (gen_rtx_SET (SImode,
				     stack_slot,
                                     gen_rtx_PLUS (SImode,
				                   stack_slot,
				                   stack_pointer_rtx)));
    }
  else
    {
      stack_slot = gen_rtx_PLUS (SImode,
			         stack_pointer_rtx,
				 GEN_INT (offset));
    }

  stack_slot = gen_rtx_MEM (SImode, stack_slot);

  insn = emit_move_insn (gen_rtx_REG (SImode, regno), stack_slot);

  return insn;
}


/* There are two possible paths for prologue expansion,
- the first is if the total frame size is < 2^15-1. In that
case all the immediates will fit into the 16-bit immediate
fields.
- the second is when the frame size is too big, in that
case an additional temporary register is used, first 
as a cfa_temp to offset the sp, second as the cfa_store
register.

See the comment above dwarf2out_frame_debug_expr in 
dwarf2out.c for more explanation of the "rules."


Case 1:
Rule #  Example Insn                       Effect
2  	addi	sp, sp, -total_frame_size  cfa.reg=sp, cfa.offset=total_frame_size
                                           cfa_store.reg=sp, cfa_store.offset=total_frame_size
12  	stw	ra, offset(sp)		   
12  	stw	r16, offset(sp)
1  	mov	fp, sp
  
Case 2: 
Rule #  Example Insn                       Effect
6 	movi	r8, total_frame_size       cfa_temp.reg=r8, cfa_temp.offset=total_frame_size
2  	sub	sp, sp, r8                 cfa.reg=sp, cfa.offset=total_frame_size
                                           cfa_store.reg=sp, cfa_store.offset=total_frame_size
5   	add	r8, r8, sp                 cfa_store.reg=r8, cfa_store.offset=0
12  	stw	ra, offset(r8)
12  	stw	r16, offset(r8)
1  	mov	fp, sp

*/

void
expand_prologue ()
{
  int i;
  HOST_WIDE_INT total_frame_size;
  int cfa_store_offset;
  rtx insn;
  rtx cfa_store_reg = 0;

  total_frame_size = compute_frame_size ();

  if (total_frame_size)
    {

      if (TOO_BIG_OFFSET (total_frame_size)) 
	{
	    /* cfa_temp and cfa_store_reg are the same register,
	       cfa_store_reg overwrites cfa_temp */
	    cfa_store_reg = gen_rtx_REG (SImode, TEMP_REG_NUM);
	    insn = emit_insn (gen_rtx_SET (SImode,
					   cfa_store_reg,
					   GEN_INT (total_frame_size)));

	    RTX_FRAME_RELATED_P (insn) = 1;


	    insn = gen_rtx_SET (SImode,
				stack_pointer_rtx,
				gen_rtx_MINUS (SImode,
					       stack_pointer_rtx,
					       cfa_store_reg));

	    insn = emit_insn (insn);
	    RTX_FRAME_RELATED_P (insn) = 1;


	    /* if there are no registers to save, I don't need to
	       create a cfa_store */
	    if (cfun->machine->frame.save_reg_size) 
	      {
		insn = gen_rtx_SET (SImode,
				    cfa_store_reg,
				    gen_rtx_PLUS (SImode,
						  cfa_store_reg,
						  stack_pointer_rtx));

		insn = emit_insn (insn);
		RTX_FRAME_RELATED_P (insn) = 1;
	      }

	    cfa_store_offset 
	      = total_frame_size 
		- (cfun->machine->frame.save_regs_offset
		   + cfun->machine->frame.save_reg_rounded);
	}
      else
	{
	    insn = gen_rtx_SET (SImode,
				stack_pointer_rtx,
				gen_rtx_PLUS (SImode,
					      stack_pointer_rtx,
					      GEN_INT (-total_frame_size)));
	    insn = emit_insn (insn);
	    RTX_FRAME_RELATED_P (insn) = 1;

	    cfa_store_reg = stack_pointer_rtx;
	    cfa_store_offset 
	      = cfun->machine->frame.save_regs_offset
		+ cfun->machine->frame.save_reg_rounded;
	}
    }

  if (MUST_SAVE_REGISTER (RA_REGNO))
    {
      cfa_store_offset -= 4;
      save_reg (RA_REGNO, cfa_store_offset, cfa_store_reg);
    }
  if (MUST_SAVE_REGISTER (FP_REGNO))
    {
      cfa_store_offset -= 4;
      save_reg (FP_REGNO, cfa_store_offset, cfa_store_reg);
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (MUST_SAVE_REGISTER (i) && i != FP_REGNO && i != RA_REGNO)
	{
	  cfa_store_offset -= 4;
	  save_reg (i, cfa_store_offset, cfa_store_reg);
	}
    }

  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_rtx_SET (SImode,
				     gen_rtx_REG (SImode, FP_REGNO),
				     gen_rtx_REG (SImode, SP_REGNO)));

      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */
  if (current_function_profile)
    emit_insn (gen_blockage ());
}

void
expand_epilogue (bool sibcall_p)
{
  rtx insn;
  int i;
  HOST_WIDE_INT total_frame_size;
  int register_store_offset;

  total_frame_size = compute_frame_size ();

  if (!sibcall_p && nios2_can_use_return_insn ())
    {
      insn = emit_jump_insn (gen_return ());
      return;
    }

  emit_insn (gen_blockage ());

  register_store_offset =
    cfun->machine->frame.save_regs_offset +
    cfun->machine->frame.save_reg_rounded;

  if (MUST_SAVE_REGISTER (RA_REGNO))
    {
      register_store_offset -= 4;
      restore_reg (RA_REGNO, register_store_offset);
    }

  if (MUST_SAVE_REGISTER (FP_REGNO))
    {
      register_store_offset -= 4;
      restore_reg (FP_REGNO, register_store_offset);
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (MUST_SAVE_REGISTER (i) && i != FP_REGNO && i != RA_REGNO)
	{
	  register_store_offset -= 4;
	  restore_reg (i, register_store_offset);
	}
    }

  if (total_frame_size)
    {
      rtx sp_adjust;

      if (TOO_BIG_OFFSET (total_frame_size))
        {
	  sp_adjust = gen_rtx_REG (SImode, TEMP_REG_NUM);
	  insn = emit_insn (gen_rtx_SET (SImode,
					 sp_adjust,
					 GEN_INT (total_frame_size)));

      	}
      else
        {
	  sp_adjust = GEN_INT (total_frame_size);
	}

      insn = gen_rtx_SET (SImode,
			  stack_pointer_rtx,
			  gen_rtx_PLUS (SImode,
					stack_pointer_rtx,
					sp_adjust));
      insn = emit_insn (insn);
    }


  if (!sibcall_p)
    {
      insn = emit_jump_insn (gen_return_from_epilogue (gen_rtx (REG, Pmode,
								RA_REGNO)));
    }
}


bool
nios2_function_ok_for_sibcall (tree a ATTRIBUTE_UNUSED, tree b ATTRIBUTE_UNUSED)
{
  return true;
}





/* ----------------------- *
 * Profiling
 * ----------------------- */

void
function_profiler (FILE *file, int labelno)
{
  fprintf (file, "\t%s mcount begin, label: .LP%d\n", 
           ASM_COMMENT_START, labelno);
  fprintf (file, "\tnextpc\tr8\n");
  fprintf (file, "\tmov\tr9, ra\n");
  fprintf (file, "\tmovhi\tr10, %%hiadj(.LP%d)\n", labelno);
  fprintf (file, "\taddi\tr10, r10, %%lo(.LP%d)\n", labelno);
  fprintf (file, "\tcall\tmcount\n");
  fprintf (file, "\tmov\tra, r9\n");
  fprintf (file, "\t%s mcount end\n", ASM_COMMENT_START);
}


/***************************************
 * Stack Layout
 ***************************************/


void
dump_frame_size (FILE *file)
{
  fprintf (file, "\t%s Current Frame Info\n", ASM_COMMENT_START);

  fprintf (file, "\t%s total_size = %ld\n", ASM_COMMENT_START,
	   cfun->machine->frame.total_size);
  fprintf (file, "\t%s var_size = %ld\n", ASM_COMMENT_START,
	   cfun->machine->frame.var_size);
  fprintf (file, "\t%s args_size = %ld\n", ASM_COMMENT_START,
	   cfun->machine->frame.args_size);
  fprintf (file, "\t%s save_reg_size = %d\n", ASM_COMMENT_START,
	   cfun->machine->frame.save_reg_size);
  fprintf (file, "\t%s save_reg_rounded = %d\n", ASM_COMMENT_START,
	   cfun->machine->frame.save_reg_rounded);
  fprintf (file, "\t%s initialized = %d\n", ASM_COMMENT_START,
	   cfun->machine->frame.initialized);
  fprintf (file, "\t%s num_regs = %d\n", ASM_COMMENT_START,
	   cfun->machine->frame.num_regs);
  fprintf (file, "\t%s save_regs_offset = %ld\n", ASM_COMMENT_START,
	   cfun->machine->frame.save_regs_offset);
  fprintf (file, "\t%s current_function_is_leaf = %d\n", ASM_COMMENT_START,
	   current_function_is_leaf);
  fprintf (file, "\t%s frame_pointer_needed = %d\n", ASM_COMMENT_START,
	   frame_pointer_needed);
  fprintf (file, "\t%s pretend_args_size = %d\n", ASM_COMMENT_START,
	   current_function_pretend_args_size);

}


/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.
*/

HOST_WIDE_INT
compute_frame_size ()
{
  unsigned int regno;
  HOST_WIDE_INT var_size;	/* # of var. bytes allocated */
  HOST_WIDE_INT total_size;	/* # bytes that the entire frame takes up */
  HOST_WIDE_INT save_reg_size;	/* # bytes needed to store callee save regs */
  HOST_WIDE_INT save_reg_rounded;	
    /* # bytes needed to store callee save regs (rounded) */
  HOST_WIDE_INT out_args_size;	/* # bytes needed for outgoing args */

  save_reg_size = 0;
  var_size = STACK_ALIGN (get_frame_size ());
  out_args_size = STACK_ALIGN (current_function_outgoing_args_size);

  total_size = var_size + out_args_size;

  /* Calculate space needed for gp registers.  */
  for (regno = 0; regno <= FIRST_PSEUDO_REGISTER; regno++)
    {
      if (MUST_SAVE_REGISTER (regno))
	{
	  save_reg_size += 4;
	}
    }

  save_reg_rounded = STACK_ALIGN (save_reg_size);
  total_size += save_reg_rounded;

  total_size += STACK_ALIGN (current_function_pretend_args_size);

  /* Save other computed information.  */
  cfun->machine->frame.total_size = total_size;
  cfun->machine->frame.var_size = var_size;
  cfun->machine->frame.args_size = current_function_outgoing_args_size;
  cfun->machine->frame.save_reg_size = save_reg_size;
  cfun->machine->frame.save_reg_rounded = save_reg_rounded;
  cfun->machine->frame.initialized = reload_completed;
  cfun->machine->frame.num_regs = save_reg_size / UNITS_PER_WORD;

  cfun->machine->frame.save_regs_offset
    = save_reg_rounded ? current_function_outgoing_args_size + var_size : 0;

  return total_size;
}


int
nios2_initial_elimination_offset (int from, int to ATTRIBUTE_UNUSED)
{
  int offset;

  /* Set OFFSET to the offset from the stack pointer.  */
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      offset = 0;
      break;

    case ARG_POINTER_REGNUM:
      compute_frame_size ();
      offset = cfun->machine->frame.total_size;
      offset -= current_function_pretend_args_size;
      break;

    case RETURN_ADDRESS_POINTER_REGNUM:
      compute_frame_size ();
      /* since the return address is always the first of the
         saved registers, return the offset to the beginning
         of the saved registers block */
      offset = cfun->machine->frame.save_regs_offset;
      break;

    default:
      abort ();
    }

  return offset;
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */
int
nios2_can_use_return_insn ()
{
  if (!reload_completed)
    return 0;

  if (regs_ever_live[RA_REGNO] || current_function_profile)
    return 0;

  if (cfun->machine->frame.initialized)
    return cfun->machine->frame.total_size == 0;

  return compute_frame_size () == 0;
}





/***************************************
 *
 ***************************************/

const char *nios2_sys_nosys_string;    /* for -msys=nosys */
const char *nios2_sys_lib_string;    /* for -msys-lib= */
const char *nios2_sys_crt0_string;    /* for -msys-crt0= */

void
override_options ()
{
  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &nios2_init_machine_status;

  nios2_section_threshold 
    = g_switch_set ? g_switch_value : NIOS2_DEFAULT_GVALUE;

  if (nios2_sys_nosys_string && *nios2_sys_nosys_string)
    {
      error ("invalid option '-msys=nosys%s'", nios2_sys_nosys_string);
    }

  /* If we don't have mul, we don't have mulx either! */
  if (!TARGET_HAS_MUL && TARGET_HAS_MULX) 
    {
      target_flags &= ~HAS_MULX_FLAG;
    }

}

void
optimization_options (int level, int size)
{
  if (level || size)
    {
      target_flags |= INLINE_MEMCPY_FLAG;
    }

  if (level >= 3 && !size)
    {
      target_flags |= FAST_SW_DIV_FLAG;
    }
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */
static struct machine_function *
nios2_init_machine_status ()
{
  return ((struct machine_function *)
	  ggc_alloc_cleared (sizeof (struct machine_function)));
}



/*****************
 * Describing Relative Costs of Operations
 *****************/

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */



static bool
nios2_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED, int *total)
{
  switch (code)
    {
      case CONST_INT:
	if (INTVAL (x) == 0)
	  {
	    *total = COSTS_N_INSNS (0);
	    return true;
	  }
	else if (SMALL_INT (INTVAL (x))
		|| SMALL_INT_UNSIGNED (INTVAL (x))
		|| UPPER16_INT (INTVAL (x)))
	  {
	    *total = COSTS_N_INSNS (2);
	    return true;
	  }
	else
	  {
	    *total = COSTS_N_INSNS (4);
	    return true;
	  }

      case LABEL_REF:
      case SYMBOL_REF:
	/* ??? gp relative stuff will fit in here */
	/* fall through */
      case CONST:
      case CONST_DOUBLE:
	{
	  *total = COSTS_N_INSNS (4);
	  return true;
	}

      case MULT:
	{
	  *total = COSTS_N_INSNS (1);
	  return false;
	}
      case SIGN_EXTEND:
	{
	  *total = COSTS_N_INSNS (3);
	  return false;
	}
      case ZERO_EXTEND:
	{
	  *total = COSTS_N_INSNS (1);
	  return false;
	}

    default:
      return false;
    }
}


/***************************************
 * INSTRUCTION SUPPORT
 *
 * These functions are used within the Machine Description to
 * handle common or complicated output and expansions from
 * instructions.
 ***************************************/

int
nios2_emit_move_sequence (rtx *operands, enum machine_mode mode)
{
  rtx to = operands[0];
  rtx from = operands[1];

  if (!register_operand (to, mode) && !reg_or_0_operand (from, mode))
    {
      if (no_new_pseudos)
	internal_error ("Trying to force_reg no_new_pseudos == 1");
      from = copy_to_mode_reg (mode, from);
    }

  operands[0] = to;
  operands[1] = from;
  return 0;
}

/* Divide Support */

/*
  If -O3 is used, we want to output a table lookup for
  divides between small numbers (both num and den >= 0
  and < 0x10). The overhead of this method in the worse
  case is 40 bytes in the text section (10 insns) and
  256 bytes in the data section. Additional divides do
  not incur additional penalties in the data section.

  Code speed is improved for small divides by about 5x
  when using this method in the worse case (~9 cycles
  vs ~45). And in the worse case divides not within the
  table are penalized by about 10% (~5 cycles vs ~45).
  However in the typical case the penalty is not as bad
  because doing the long divide in only 45 cycles is
  quite optimistic.

  ??? It would be nice to have some benchmarks other
  than Dhrystone to back this up.

  This bit of expansion is to create this instruction
  sequence as rtl.
	or	$8, $4, $5
	slli	$9, $4, 4
	cmpgeui	$3, $8, 16
	beq	$3, $0, .L3
	or	$10, $9, $5
	add	$12, $11, divide_table
	ldbu	$2, 0($12)
	br	.L1
.L3:
	call	slow_div
.L1:
#	continue here with result in $2

  ??? Ideally I would like the emit libcall block to contain
  all of this code, but I don't know how to do that. What it
  means is that if the divide can be eliminated, it may not
  completely disappear.

  ??? The __divsi3_table label should ideally be moved out
  of this block and into a global. If it is placed into the
  sdata section we can save even more cycles by doing things
  gp relative.
*/
int
nios2_emit_expensive_div (rtx *operands, enum machine_mode mode)
{
  rtx or_result, shift_left_result;
  rtx lookup_value;
  rtx lab1, lab3;
  rtx insns;
  rtx libfunc;
  rtx final_result;
  rtx tmp;

  /* it may look a little generic, but only SImode
     is supported for now */
  if (mode != SImode)
    abort ();

  libfunc = sdiv_optab->handlers[(int) SImode].libfunc;



  lab1 = gen_label_rtx ();
  lab3 = gen_label_rtx ();

  or_result = expand_simple_binop (SImode, IOR,
				   operands[1], operands[2],
				   0, 0, OPTAB_LIB_WIDEN);

  emit_cmp_and_jump_insns (or_result, GEN_INT (15), GTU, 0,
			   GET_MODE (or_result), 0, lab3);
  JUMP_LABEL (get_last_insn ()) = lab3;

  shift_left_result = expand_simple_binop (SImode, ASHIFT,
					   operands[1], GEN_INT (4),
					   0, 0, OPTAB_LIB_WIDEN);

  lookup_value = expand_simple_binop (SImode, IOR,
				      shift_left_result, operands[2],
				      0, 0, OPTAB_LIB_WIDEN);

  convert_move (operands[0],
		gen_rtx (MEM, QImode,
			 gen_rtx (PLUS, SImode,
				  lookup_value,
				  gen_rtx_SYMBOL_REF (SImode, "__divsi3_table"))),
		1);


  tmp = emit_jump_insn (gen_jump (lab1));
  JUMP_LABEL (tmp) = lab1;
  emit_barrier ();

  emit_label (lab3);
  LABEL_NUSES (lab3) = 1;

  start_sequence ();
  final_result = emit_library_call_value (libfunc, NULL_RTX,
					  LCT_CONST, SImode, 2,
					  operands[1], SImode,
					  operands[2], SImode);


  insns = get_insns ();
  end_sequence ();
  emit_libcall_block (insns, operands[0], final_result,
		      gen_rtx (DIV, SImode, operands[1], operands[2]));

  emit_label (lab1);
  LABEL_NUSES (lab1) = 1;
  return 1;
}

/* Branches/Compares */

/* the way of handling branches/compares
   in gcc is heavily borrowed from MIPS */

enum internal_test
{
  ITEST_EQ,
  ITEST_NE,
  ITEST_GT,
  ITEST_GE,
  ITEST_LT,
  ITEST_LE,
  ITEST_GTU,
  ITEST_GEU,
  ITEST_LTU,
  ITEST_LEU,
  ITEST_MAX
};

static enum internal_test map_test_to_internal_test (enum rtx_code);

/* Cached operands, and operator to compare for use in set/branch/trap
   on condition codes.  */
rtx branch_cmp[2];
enum cmp_type branch_type;

/* Make normal rtx_code into something we can index from an array */

static enum internal_test
map_test_to_internal_test (enum rtx_code test_code)
{
  enum internal_test test = ITEST_MAX;

  switch (test_code)
    {
    case EQ:
      test = ITEST_EQ;
      break;
    case NE:
      test = ITEST_NE;
      break;
    case GT:
      test = ITEST_GT;
      break;
    case GE:
      test = ITEST_GE;
      break;
    case LT:
      test = ITEST_LT;
      break;
    case LE:
      test = ITEST_LE;
      break;
    case GTU:
      test = ITEST_GTU;
      break;
    case GEU:
      test = ITEST_GEU;
      break;
    case LTU:
      test = ITEST_LTU;
      break;
    case LEU:
      test = ITEST_LEU;
      break;
    default:
      break;
    }

  return test;
}

/* Generate the code to compare (and possibly branch) two integer values
   TEST_CODE is the comparison code we are trying to emulate 
     (or implement directly)
   RESULT is where to store the result of the comparison, 
     or null to emit a branch
   CMP0 CMP1 are the two comparison operands
   DESTINATION is the destination of the branch, or null to only compare
   */

void
gen_int_relational (enum rtx_code test_code, /* relational test (EQ, etc) */
		    rtx result,		/* result to store comp. or 0 if branch */
		    rtx cmp0,		/* first operand to compare */
		    rtx cmp1,		/* second operand to compare */
		    rtx destination)	/* destination of the branch, or 0 if compare */
{
  struct cmp_info
  {
    /* for register (or 0) compares */
    enum rtx_code test_code_reg;	/* code to use in instruction (LT vs. LTU) */
    int reverse_regs;		/* reverse registers in test */

    /* for immediate compares */
    enum rtx_code test_code_const;	
         /* code to use in instruction (LT vs. LTU) */
    int const_low;		/* low bound of constant we can accept */
    int const_high;		/* high bound of constant we can accept */
    int const_add;		/* constant to add */

    /* generic info */
    int unsignedp;		/* != 0 for unsigned comparisons.  */
  };

  static const struct cmp_info info[(int) ITEST_MAX] = {

    {EQ, 0, EQ, -32768, 32767, 0, 0}, /* EQ  */
    {NE, 0, NE, -32768, 32767, 0, 0}, /* NE  */

    {LT, 1, GE, -32769, 32766, 1, 0}, /* GT  */
    {GE, 0, GE, -32768, 32767, 0, 0}, /* GE  */
    {LT, 0, LT, -32768, 32767, 0, 0}, /* LT  */
    {GE, 1, LT, -32769, 32766, 1, 0}, /* LE  */

    {LTU, 1, GEU, 0, 65534, 1, 0}, /* GTU */
    {GEU, 0, GEU, 0, 65535, 0, 0}, /* GEU */
    {LTU, 0, LTU, 0, 65535, 0, 0}, /* LTU */
    {GEU, 1, LTU, 0, 65534, 1, 0}, /* LEU */
  };

  enum internal_test test;
  enum machine_mode mode;
  const struct cmp_info *p_info;
  int branch_p;




  test = map_test_to_internal_test (test_code);
  if (test == ITEST_MAX)
    abort ();

  p_info = &info[(int) test];

  mode = GET_MODE (cmp0);
  if (mode == VOIDmode)
    mode = GET_MODE (cmp1);

  branch_p = (destination != 0);

  /* We can't, under any circumstances, have const_ints in cmp0
     ??? Actually we could have const0 */
  if (GET_CODE (cmp0) == CONST_INT)
    cmp0 = force_reg (mode, cmp0);

  /* if the comparison is against an int not in legal range
     move it into a register */
  if (GET_CODE (cmp1) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL (cmp1);

      if (value < p_info->const_low || value > p_info->const_high)
	cmp1 = force_reg (mode, cmp1);
    }

  /* Comparison to constants, may involve adding 1 to change a GT into GE.
     Comparison between two registers, may involve switching operands.  */
  if (GET_CODE (cmp1) == CONST_INT)
    {
      if (p_info->const_add != 0)
	{
	  HOST_WIDE_INT new = INTVAL (cmp1) + p_info->const_add;

	  /* If modification of cmp1 caused overflow,
	     we would get the wrong answer if we follow the usual path;
	     thus, x > 0xffffffffU would turn into x > 0U.  */
	  if ((p_info->unsignedp
	       ? (unsigned HOST_WIDE_INT) new >
	       (unsigned HOST_WIDE_INT) INTVAL (cmp1)
	       : new > INTVAL (cmp1)) != (p_info->const_add > 0))
	    {
	      /* ??? This case can never happen with the current numbers,
	         but I am paranoid and would rather an abort than
	         a bug I will never find */
	      abort ();
	    }
	  else
	    cmp1 = GEN_INT (new);
	}
    }

  else if (p_info->reverse_regs)
    {
      rtx temp = cmp0;
      cmp0 = cmp1;
      cmp1 = temp;
    }



  if (branch_p)
    {
      if (register_operand (cmp0, mode) && register_operand (cmp1, mode))
	{
	  rtx insn;
	  rtx cond = gen_rtx (p_info->test_code_reg, mode, cmp0, cmp1);
	  rtx label = gen_rtx_LABEL_REF (VOIDmode, destination);

	  insn = gen_rtx_SET (VOIDmode, pc_rtx,
			      gen_rtx_IF_THEN_ELSE (VOIDmode,
						    cond, label, pc_rtx));
	  emit_jump_insn (insn);
	}
      else
	{
	  rtx cond, label;

	  result = gen_reg_rtx (mode);

	  emit_move_insn (result,
			  gen_rtx (p_info->test_code_const, mode, cmp0,
				   cmp1));

	  cond = gen_rtx (NE, mode, result, const0_rtx);
	  label = gen_rtx_LABEL_REF (VOIDmode, destination);

	  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				       gen_rtx_IF_THEN_ELSE (VOIDmode,
							     cond,
							     label, pc_rtx)));
	}
    }
  else
    {
      if (register_operand (cmp0, mode) && register_operand (cmp1, mode))
	{
	  emit_move_insn (result,
			  gen_rtx (p_info->test_code_reg, mode, cmp0, cmp1));
	}
      else
	{
	  emit_move_insn (result,
			  gen_rtx (p_info->test_code_const, mode, cmp0,
				   cmp1));
	}
    }

}


/* ??? For now conditional moves are only supported
   when the mode of the operands being compared are
   the same as the ones being moved */

void
gen_conditional_move (rtx *operands, enum machine_mode mode)
{
  rtx insn, cond;
  rtx cmp_reg = gen_reg_rtx (mode);
  enum rtx_code cmp_code = GET_CODE (operands[1]);
  enum rtx_code move_code = EQ;

  /* emit a comparison if it is not "simple".
     Simple comparisons are X eq 0 and X ne 0 */
  if ((cmp_code == EQ || cmp_code == NE) && branch_cmp[1] == const0_rtx)
    {
      cmp_reg = branch_cmp[0];
      move_code = cmp_code;
    }
  else if ((cmp_code == EQ || cmp_code == NE) && branch_cmp[0] == const0_rtx)
    {
      cmp_reg = branch_cmp[1];
      move_code = cmp_code == EQ ? NE : EQ;
    }
  else
    gen_int_relational (cmp_code, cmp_reg, branch_cmp[0], branch_cmp[1],
			NULL_RTX);

  cond = gen_rtx (move_code, VOIDmode, cmp_reg, CONST0_RTX (mode));
  insn = gen_rtx_SET (mode, operands[0],
		      gen_rtx_IF_THEN_ELSE (mode,
					    cond, operands[2], operands[3]));
  emit_insn (insn);
}

/*******************
 * Addressing Modes
 *******************/

int
nios2_legitimate_address (rtx operand, enum machine_mode mode ATTRIBUTE_UNUSED, 
                          int strict)
{
  int ret_val = 0;

  switch (GET_CODE (operand))
    {
      /* direct.  */
    case SYMBOL_REF:
      if (SYMBOL_REF_IN_NIOS2_SMALL_DATA_P (operand))
        {
          ret_val = 1;
          break;
	}
      /* else, fall through */
    case LABEL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
      /* ??? In here I need to add gp addressing */
      ret_val = 0;

      break;

      /* Register indirect.  */
    case REG:
      ret_val = REG_OK_FOR_BASE_P2 (operand, strict);
      break;

      /* Register indirect with displacement */
    case PLUS:
      {
	rtx op0 = XEXP (operand, 0);
	rtx op1 = XEXP (operand, 1);

	if (REG_P (op0) && REG_P (op1))
	  ret_val = 0;
	else if (REG_P (op0) && CONSTANT_P (op1))
	  ret_val = REG_OK_FOR_BASE_P2 (op0, strict)
	    && SMALL_INT (INTVAL (op1));
	else if (REG_P (op1) && CONSTANT_P (op0))
	  ret_val = REG_OK_FOR_BASE_P2 (op1, strict)
	    && SMALL_INT (INTVAL (op0));
	else
	  ret_val = 0;
      }
      break;

    default:
      ret_val = 0;
      break;
    }

  return ret_val;
}

/* Return true if EXP should be placed in the small data section.  */

static bool
nios2_in_small_data_p (tree exp)
{
  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (exp) == STRING_CST)
    return false;

  if (TREE_CODE (exp) == VAR_DECL && DECL_SECTION_NAME (exp))
    {
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (exp));
      /* ??? these string names need moving into 
         an array in some header file */
      if (nios2_section_threshold > 0
          && (strcmp (section, ".sbss") == 0
	      || strncmp (section, ".sbss.", 6) == 0
	      || strcmp (section, ".sdata") == 0
	      || strncmp (section, ".sdata.", 7) == 0))
	return true;
    }
  else if (TREE_CODE (exp) == VAR_DECL)
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

      /* If this is an incomplete type with size 0, then we can't put it
         in sdata because it might be too big when completed.  */
      if (size > 0 && size <= nios2_section_threshold)
	return true;
    }

  return false;
}

static void
nios2_encode_section_info (tree decl, rtx rtl, int first)
{

  rtx symbol;
  int flags;

  default_encode_section_info (decl, rtl, first);
  
  /* Careful not to prod global register variables.  */
  if (GET_CODE (rtl) != MEM)
    return;
  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  flags = SYMBOL_REF_FLAGS (symbol);
    
  /* We don't want weak variables to be addressed with gp in case they end up with
     value 0 which is not within 2^15 of $gp */
  if (DECL_P (decl) && DECL_WEAK (decl))
    flags |= SYMBOL_FLAG_WEAK_DECL;

  SYMBOL_REF_FLAGS (symbol) = flags;
}


static unsigned int
nios2_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags;

  flags = default_section_type_flags (decl, name, reloc);

  /* ??? these string names need moving into an array in some header file */
  if (strcmp (name, ".sbss") == 0
      || strncmp (name, ".sbss.", 6) == 0
      || strcmp (name, ".sdata") == 0
      || strncmp (name, ".sdata.", 7) == 0)
    flags |= SECTION_SMALL;

  return flags;
}




/*****************************************
 * Defining the Output Assembler Language
 *****************************************/

/* -------------- *
 * Output of Data
 * -------------- */


/* -------------------------------- *
 * Output of Assembler Instructions
 * -------------------------------- */


/* print the operand OP to file stream
   FILE modified by LETTER. LETTER
   can be one of:
     i: print "i" if OP is an immediate, except 0
     o: print "io" if OP is volatile

     z: for const0_rtx print $0 instead of 0
     H: for %hiadj
     L: for %lo
     U: for upper half of 32 bit value
 */

void
nios2_print_operand (FILE *file, rtx op, int letter)
{

  switch (letter)
    {
    case 'i':
      if (CONSTANT_P (op) && (op != const0_rtx))
	fprintf (file, "i");
      return;

    case 'o':
      if (GET_CODE (op) == MEM
          && ((MEM_VOLATILE_P (op) && !TARGET_CACHE_VOLATILE)
              || TARGET_BYPASS_CACHE))
	fprintf (file, "io");
      return;

    default:
      break;
    }

  if (comparison_operator (op, VOIDmode))
    {
      if (letter == 0)
	{
	  fprintf (file, "%s", GET_RTX_NAME (GET_CODE (op)));
	  return;
	}
    }


  switch (GET_CODE (op))
    {
    case REG:
      if (letter == 0 || letter == 'z')
	{
	  fprintf (file, "%s", reg_names[REGNO (op)]);
	  return;
	}

    case CONST_INT:
      if (INTVAL (op) == 0 && letter == 'z')
	{
	  fprintf (file, "zero");
	  return;
	}
      else if (letter == 'U')
	{
	  HOST_WIDE_INT val = INTVAL (op);
	  rtx new_op;
	  val = (val / 65536) & 0xFFFF;
	  new_op = GEN_INT (val);
	  output_addr_const (file, new_op);
	  return;
	}

      /* else, fall through */
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      if (letter == 0 || letter == 'z')
	{
	  output_addr_const (file, op);
	  return;
	}
      else if (letter == 'H')
	{
	  fprintf (file, "%%hiadj(");
	  output_addr_const (file, op);
	  fprintf (file, ")");
	  return;
	}
      else if (letter == 'L')
	{
	  fprintf (file, "%%lo(");
	  output_addr_const (file, op);
	  fprintf (file, ")");
	  return;
	}


    case SUBREG:
    case MEM:
      if (letter == 0)
	{
	  output_address (op);
	  return;
	}

    case CODE_LABEL:
      if (letter == 0)
	{
	  output_addr_const (file, op);
	  return;
	}

    default:
      break;
    }

  fprintf (stderr, "Missing way to print (%c) ", letter);
  debug_rtx (op);
  abort ();
}

static int gprel_constant (rtx);

static int
gprel_constant (rtx op)
{
  if (GET_CODE (op) == SYMBOL_REF
      && SYMBOL_REF_IN_NIOS2_SMALL_DATA_P (op))
    {
      return 1;
    }
  else if (GET_CODE (op) == CONST
           && GET_CODE (XEXP (op, 0)) == PLUS)
    {
      return gprel_constant (XEXP (XEXP (op, 0), 0));
    }
  else
    {
      return 0;
    }
}

void
nios2_print_operand_address (FILE *file, rtx op)
{
  switch (GET_CODE (op))
    {
    case CONST:
    case CONST_INT:
    case LABEL_REF:
    case CONST_DOUBLE:
    case SYMBOL_REF:
      if (gprel_constant (op))
        {
          fprintf (file, "%%gprel(");
          output_addr_const (file, op);
          fprintf (file, ")(%s)", reg_names[GP_REGNO]);
          return;
        }

      break;

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (REG_P (op0) && CONSTANT_P (op1))
	  {
	    output_addr_const (file, op1);
	    fprintf (file, "(%s)", reg_names[REGNO (op0)]);
	    return;
	  }
	else if (REG_P (op1) && CONSTANT_P (op0))
	  {
	    output_addr_const (file, op0);
	    fprintf (file, "(%s)", reg_names[REGNO (op1)]);
	    return;
	  }
      }
      break;

    case REG:
      fprintf (file, "0(%s)", reg_names[REGNO (op)]);
      return;

    case MEM:
      {
	rtx base = XEXP (op, 0);
	PRINT_OPERAND_ADDRESS (file, base);
	return;
      }
    default:
      break;
    }

  fprintf (stderr, "Missing way to print address\n");
  debug_rtx (op);
  abort ();
}





/****************************
 * Predicates
 ****************************/

int
arith_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT (INTVAL (op)))
    return 1;

  return register_operand (op, mode);
}

int
uns_arith_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT_UNSIGNED (INTVAL (op)))
    return 1;

  return register_operand (op, mode);
}

int
logical_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT
      && (SMALL_INT_UNSIGNED (INTVAL (op)) || UPPER16_INT (INTVAL (op))))
    return 1;

  return register_operand (op, mode);
}

int
shift_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT && SHIFT_INT (INTVAL (op)))
    return 1;

  return register_operand (op, mode);
}

int
rdwrctl_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return GET_CODE (op) == CONST_INT && RDWRCTL_INT (INTVAL (op));
}

/* Return truth value of whether OP is a register or the constant 0. */

int
reg_or_0_operand (rtx op, enum machine_mode mode)
{
  switch (GET_CODE (op))
    {
    case CONST_INT:
      return INTVAL (op) == 0;

    case CONST_DOUBLE:
      return op == CONST0_RTX (mode);

    default:
      break;
    }

  return register_operand (op, mode);
}


int
equality_op (rtx op, enum machine_mode mode)
{
  if (mode != GET_MODE (op))
    return 0;

  return GET_CODE (op) == EQ || GET_CODE (op) == NE;
}

int
custom_insn_opcode (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return GET_CODE (op) == CONST_INT && CUSTOM_INSN_OPCODE (INTVAL (op));
}







/*****************************************************************************
**
** instruction scheduler
**
*****************************************************************************/
static int
nios2_use_dfa_pipeline_interface ()
{
  return 1;
}


static int
nios2_issue_rate ()
{
#ifdef MAX_DFA_ISSUE_RATE
  return MAX_DFA_ISSUE_RATE;
#else
  return 1;
#endif
}


const char *
asm_output_opcode (FILE *file ATTRIBUTE_UNUSED, 
                   const char *ptr ATTRIBUTE_UNUSED)
{
  const char *p;

  p = ptr;
  return ptr;
}



/*****************************************************************************
**
** function arguments
**
*****************************************************************************/

void
init_cumulative_args (CUMULATIVE_ARGS *cum, 
                      tree fntype ATTRIBUTE_UNUSED, 
                      rtx libname ATTRIBUTE_UNUSED, 
                      tree fndecl ATTRIBUTE_UNUSED, 
                      int n_named_args ATTRIBUTE_UNUSED)
{
  cum->regs_used = 0;
}


/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode, 
                      tree type ATTRIBUTE_UNUSED, int named ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT param_size;

  if (mode == BLKmode)
    {
      param_size = int_size_in_bytes (type);
      if (param_size < 0)
	internal_error
	  ("Do not know how to handle large structs or variable length types");
    }
  else
    {
      param_size = GET_MODE_SIZE (mode);
    }

  /* convert to words (round up) */
  param_size = (3 + param_size) / 4;

  if (cum->regs_used + param_size > NUM_ARG_REGS)
    {
      cum->regs_used = NUM_ARG_REGS;
    }
  else
    {
      cum->regs_used += param_size;
    }

  return;
}

/* Define where to put the arguments to a function.  Value is zero to
   push the argument on the stack, or a hard register in which to
   store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
   This is null for libcalls where that information may
   not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
   the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
   (otherwise it is an extra parameter matching an ellipsis).  */
rtx
function_arg (const CUMULATIVE_ARGS *cum, enum machine_mode mode, 
              tree type ATTRIBUTE_UNUSED, int named ATTRIBUTE_UNUSED)
{
  rtx return_rtx = NULL_RTX;

  if (cum->regs_used < NUM_ARG_REGS)
    {
      return_rtx = gen_rtx_REG (mode, FIRST_ARG_REGNO + cum->regs_used);
    }

  return return_rtx;
}

int
function_arg_partial_nregs (const CUMULATIVE_ARGS *cum,
                            enum machine_mode mode, tree type, 
                            int named ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT param_size;

  if (mode == BLKmode)
    {
      param_size = int_size_in_bytes (type);
      if (param_size < 0)
	internal_error
	  ("Do not know how to handle large structs or variable length types");
    }
  else
    {
      param_size = GET_MODE_SIZE (mode);
    }

  /* convert to words (round up) */
  param_size = (3 + param_size) / 4;

  if (cum->regs_used < NUM_ARG_REGS
      && cum->regs_used + param_size > NUM_ARG_REGS)
    {
      return NUM_ARG_REGS - cum->regs_used;
    }
  else
    {
      return 0;
    }
}


int
nios2_return_in_memory (tree type)
{
  int res = ((int_size_in_bytes (type) > (2 * UNITS_PER_WORD))
  	     || (int_size_in_bytes (type) == -1));

  return res;
}

/* ??? It may be possible to eliminate the copyback and implement
       my own va_arg type, but that is more work for now. */
int
nios2_setup_incoming_varargs (const CUMULATIVE_ARGS *cum, 
                              enum machine_mode mode, tree type, 
                              int no_rtl)
{
  CUMULATIVE_ARGS local_cum;
  int regs_to_push;

  local_cum = *cum;
  FUNCTION_ARG_ADVANCE (local_cum, mode, type, 1);

  regs_to_push = NUM_ARG_REGS - local_cum.regs_used;

  if (!no_rtl)
    {
      if (regs_to_push > 0)
	{
	  rtx ptr, mem;

	  ptr = virtual_incoming_args_rtx;
	  mem = gen_rtx_MEM (BLKmode, ptr);

	  /* va_arg is an array access in this case, which causes
	     it to get MEM_IN_STRUCT_P set.  We must set it here
	     so that the insn scheduler won't assume that these
	     stores can't possibly overlap with the va_arg loads.  */
	  MEM_SET_IN_STRUCT_P (mem, 1);

	  emit_insn (gen_blockage ());
	  move_block_from_reg (local_cum.regs_used + FIRST_ARG_REGNO, mem,
			       regs_to_push);
	  emit_insn (gen_blockage ());
	}
    }

  return regs_to_push * UNITS_PER_WORD;

}



/*****************************************************************************
**
** builtins
**
** This method for handling builtins is from CSP where _many_ more types of
** expanders have already been written. Check there first before writing
** new ones.
**
*****************************************************************************/

enum nios2_builtins
{
  NIOS2_BUILTIN_LDBIO,
  NIOS2_BUILTIN_LDBUIO,
  NIOS2_BUILTIN_LDHIO,
  NIOS2_BUILTIN_LDHUIO,
  NIOS2_BUILTIN_LDWIO,
  NIOS2_BUILTIN_STBIO,
  NIOS2_BUILTIN_STHIO,
  NIOS2_BUILTIN_STWIO,
  NIOS2_BUILTIN_SYNC,
  NIOS2_BUILTIN_RDCTL,
  NIOS2_BUILTIN_WRCTL,

  NIOS2_BUILTIN_CUSTOM_N,
  NIOS2_BUILTIN_CUSTOM_NI,
  NIOS2_BUILTIN_CUSTOM_NF,
  NIOS2_BUILTIN_CUSTOM_NP,
  NIOS2_BUILTIN_CUSTOM_NII,
  NIOS2_BUILTIN_CUSTOM_NIF,
  NIOS2_BUILTIN_CUSTOM_NIP,
  NIOS2_BUILTIN_CUSTOM_NFI,
  NIOS2_BUILTIN_CUSTOM_NFF,
  NIOS2_BUILTIN_CUSTOM_NFP,
  NIOS2_BUILTIN_CUSTOM_NPI,
  NIOS2_BUILTIN_CUSTOM_NPF,
  NIOS2_BUILTIN_CUSTOM_NPP,
  NIOS2_BUILTIN_CUSTOM_IN,
  NIOS2_BUILTIN_CUSTOM_INI,
  NIOS2_BUILTIN_CUSTOM_INF,
  NIOS2_BUILTIN_CUSTOM_INP,
  NIOS2_BUILTIN_CUSTOM_INII,
  NIOS2_BUILTIN_CUSTOM_INIF,
  NIOS2_BUILTIN_CUSTOM_INIP,
  NIOS2_BUILTIN_CUSTOM_INFI,
  NIOS2_BUILTIN_CUSTOM_INFF,
  NIOS2_BUILTIN_CUSTOM_INFP,
  NIOS2_BUILTIN_CUSTOM_INPI,
  NIOS2_BUILTIN_CUSTOM_INPF,
  NIOS2_BUILTIN_CUSTOM_INPP,
  NIOS2_BUILTIN_CUSTOM_FN,
  NIOS2_BUILTIN_CUSTOM_FNI,
  NIOS2_BUILTIN_CUSTOM_FNF,
  NIOS2_BUILTIN_CUSTOM_FNP,
  NIOS2_BUILTIN_CUSTOM_FNII,
  NIOS2_BUILTIN_CUSTOM_FNIF,
  NIOS2_BUILTIN_CUSTOM_FNIP,
  NIOS2_BUILTIN_CUSTOM_FNFI,
  NIOS2_BUILTIN_CUSTOM_FNFF,
  NIOS2_BUILTIN_CUSTOM_FNFP,
  NIOS2_BUILTIN_CUSTOM_FNPI,
  NIOS2_BUILTIN_CUSTOM_FNPF,
  NIOS2_BUILTIN_CUSTOM_FNPP,
  NIOS2_BUILTIN_CUSTOM_PN,
  NIOS2_BUILTIN_CUSTOM_PNI,
  NIOS2_BUILTIN_CUSTOM_PNF,
  NIOS2_BUILTIN_CUSTOM_PNP,
  NIOS2_BUILTIN_CUSTOM_PNII,
  NIOS2_BUILTIN_CUSTOM_PNIF,
  NIOS2_BUILTIN_CUSTOM_PNIP,
  NIOS2_BUILTIN_CUSTOM_PNFI,
  NIOS2_BUILTIN_CUSTOM_PNFF,
  NIOS2_BUILTIN_CUSTOM_PNFP,
  NIOS2_BUILTIN_CUSTOM_PNPI,
  NIOS2_BUILTIN_CUSTOM_PNPF,
  NIOS2_BUILTIN_CUSTOM_PNPP,


  LIM_NIOS2_BUILTINS
};

struct builtin_description
{
    const enum insn_code icode;
    const char *const name;
    const enum nios2_builtins code;
    const tree *type;
    rtx (* expander) PARAMS ((const struct builtin_description *,
                              tree, rtx, rtx, enum machine_mode, int));
};

static rtx nios2_expand_STXIO (const struct builtin_description *, 
                               tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_LDXIO (const struct builtin_description *, 
                               tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_sync (const struct builtin_description *, 
                              tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_rdctl (const struct builtin_description *, 
                               tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_wrctl (const struct builtin_description *, 
                               tree, rtx, rtx, enum machine_mode, int);

static rtx nios2_expand_custom_n (const struct builtin_description *, 
                                  tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_custom_Xn (const struct builtin_description *, 
                                   tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_custom_nX (const struct builtin_description *, 
                                   tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_custom_XnX (const struct builtin_description *, 
                                    tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_custom_nXX (const struct builtin_description *, 
                                    tree, rtx, rtx, enum machine_mode, int);
static rtx nios2_expand_custom_XnXX (const struct builtin_description *, 
                                     tree, rtx, rtx, enum machine_mode, int);

static tree endlink;

/* int fn (volatile const void *)
 */
static tree int_ftype_volatile_const_void_p;

/* int fn (int)
 */
static tree int_ftype_int;

/* void fn (int, int)
 */
static tree void_ftype_int_int;

/* void fn (volatile void *, int)
 */
static tree void_ftype_volatile_void_p_int;

/* void fn (void)
 */
static tree void_ftype_void;

static tree custom_n;
static tree custom_ni;
static tree custom_nf;
static tree custom_np;
static tree custom_nii;
static tree custom_nif;
static tree custom_nip;
static tree custom_nfi;
static tree custom_nff;
static tree custom_nfp;
static tree custom_npi;
static tree custom_npf;
static tree custom_npp;
static tree custom_in;
static tree custom_ini;
static tree custom_inf;
static tree custom_inp;
static tree custom_inii;
static tree custom_inif;
static tree custom_inip;
static tree custom_infi;
static tree custom_inff;
static tree custom_infp;
static tree custom_inpi;
static tree custom_inpf;
static tree custom_inpp;
static tree custom_fn;
static tree custom_fni;
static tree custom_fnf;
static tree custom_fnp;
static tree custom_fnii;
static tree custom_fnif;
static tree custom_fnip;
static tree custom_fnfi;
static tree custom_fnff;
static tree custom_fnfp;
static tree custom_fnpi;
static tree custom_fnpf;
static tree custom_fnpp;
static tree custom_pn;
static tree custom_pni;
static tree custom_pnf;
static tree custom_pnp;
static tree custom_pnii;
static tree custom_pnif;
static tree custom_pnip;
static tree custom_pnfi;
static tree custom_pnff;
static tree custom_pnfp;
static tree custom_pnpi;
static tree custom_pnpf;
static tree custom_pnpp;


static const struct builtin_description bdesc[] = {
    {CODE_FOR_ldbio, "__builtin_ldbio", NIOS2_BUILTIN_LDBIO, &int_ftype_volatile_const_void_p, nios2_expand_LDXIO},
    {CODE_FOR_ldbuio, "__builtin_ldbuio", NIOS2_BUILTIN_LDBUIO, &int_ftype_volatile_const_void_p, nios2_expand_LDXIO},
    {CODE_FOR_ldhio, "__builtin_ldhio", NIOS2_BUILTIN_LDHIO, &int_ftype_volatile_const_void_p, nios2_expand_LDXIO},
    {CODE_FOR_ldhuio, "__builtin_ldhuio", NIOS2_BUILTIN_LDHUIO, &int_ftype_volatile_const_void_p, nios2_expand_LDXIO},
    {CODE_FOR_ldwio, "__builtin_ldwio", NIOS2_BUILTIN_LDWIO, &int_ftype_volatile_const_void_p, nios2_expand_LDXIO},

    {CODE_FOR_stbio, "__builtin_stbio", NIOS2_BUILTIN_STBIO, &void_ftype_volatile_void_p_int, nios2_expand_STXIO},
    {CODE_FOR_sthio, "__builtin_sthio", NIOS2_BUILTIN_STHIO, &void_ftype_volatile_void_p_int, nios2_expand_STXIO},
    {CODE_FOR_stwio, "__builtin_stwio", NIOS2_BUILTIN_STWIO, &void_ftype_volatile_void_p_int, nios2_expand_STXIO},

    {CODE_FOR_sync, "__builtin_sync", NIOS2_BUILTIN_SYNC, &void_ftype_void, nios2_expand_sync},
    {CODE_FOR_rdctl, "__builtin_rdctl", NIOS2_BUILTIN_RDCTL, &int_ftype_int, nios2_expand_rdctl},
    {CODE_FOR_wrctl, "__builtin_wrctl", NIOS2_BUILTIN_WRCTL, &void_ftype_int_int, nios2_expand_wrctl},

    {CODE_FOR_custom_n, "__builtin_custom_n", NIOS2_BUILTIN_CUSTOM_N, &custom_n, nios2_expand_custom_n},
    {CODE_FOR_custom_ni, "__builtin_custom_ni", NIOS2_BUILTIN_CUSTOM_NI, &custom_ni, nios2_expand_custom_nX},
    {CODE_FOR_custom_nf, "__builtin_custom_nf", NIOS2_BUILTIN_CUSTOM_NF, &custom_nf, nios2_expand_custom_nX},
    {CODE_FOR_custom_np, "__builtin_custom_np", NIOS2_BUILTIN_CUSTOM_NP, &custom_np, nios2_expand_custom_nX},
    {CODE_FOR_custom_nii, "__builtin_custom_nii", NIOS2_BUILTIN_CUSTOM_NII, &custom_nii, nios2_expand_custom_nXX},
    {CODE_FOR_custom_nif, "__builtin_custom_nif", NIOS2_BUILTIN_CUSTOM_NIF, &custom_nif, nios2_expand_custom_nXX},
    {CODE_FOR_custom_nip, "__builtin_custom_nip", NIOS2_BUILTIN_CUSTOM_NIP, &custom_nip, nios2_expand_custom_nXX},
    {CODE_FOR_custom_nfi, "__builtin_custom_nfi", NIOS2_BUILTIN_CUSTOM_NFI, &custom_nfi, nios2_expand_custom_nXX},
    {CODE_FOR_custom_nff, "__builtin_custom_nff", NIOS2_BUILTIN_CUSTOM_NFF, &custom_nff, nios2_expand_custom_nXX},
    {CODE_FOR_custom_nfp, "__builtin_custom_nfp", NIOS2_BUILTIN_CUSTOM_NFP, &custom_nfp, nios2_expand_custom_nXX},
    {CODE_FOR_custom_npi, "__builtin_custom_npi", NIOS2_BUILTIN_CUSTOM_NPI, &custom_npi, nios2_expand_custom_nXX},
    {CODE_FOR_custom_npf, "__builtin_custom_npf", NIOS2_BUILTIN_CUSTOM_NPF, &custom_npf, nios2_expand_custom_nXX},
    {CODE_FOR_custom_npp, "__builtin_custom_npp", NIOS2_BUILTIN_CUSTOM_NPP, &custom_npp, nios2_expand_custom_nXX},
    {CODE_FOR_custom_in, "__builtin_custom_in", NIOS2_BUILTIN_CUSTOM_IN, &custom_in, nios2_expand_custom_Xn},
    {CODE_FOR_custom_ini, "__builtin_custom_ini", NIOS2_BUILTIN_CUSTOM_INI, &custom_ini, nios2_expand_custom_XnX},
    {CODE_FOR_custom_inf, "__builtin_custom_inf", NIOS2_BUILTIN_CUSTOM_INF, &custom_inf, nios2_expand_custom_XnX},
    {CODE_FOR_custom_inp, "__builtin_custom_inp", NIOS2_BUILTIN_CUSTOM_INP, &custom_inp, nios2_expand_custom_XnX},
    {CODE_FOR_custom_inii, "__builtin_custom_inii", NIOS2_BUILTIN_CUSTOM_INII, &custom_inii, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_inif, "__builtin_custom_inif", NIOS2_BUILTIN_CUSTOM_INIF, &custom_inif, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_inip, "__builtin_custom_inip", NIOS2_BUILTIN_CUSTOM_INIP, &custom_inip, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_infi, "__builtin_custom_infi", NIOS2_BUILTIN_CUSTOM_INFI, &custom_infi, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_inff, "__builtin_custom_inff", NIOS2_BUILTIN_CUSTOM_INFF, &custom_inff, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_infp, "__builtin_custom_infp", NIOS2_BUILTIN_CUSTOM_INFP, &custom_infp, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_inpi, "__builtin_custom_inpi", NIOS2_BUILTIN_CUSTOM_INPI, &custom_inpi, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_inpf, "__builtin_custom_inpf", NIOS2_BUILTIN_CUSTOM_INPF, &custom_inpf, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_inpp, "__builtin_custom_inpp", NIOS2_BUILTIN_CUSTOM_INPP, &custom_inpp, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fn, "__builtin_custom_fn", NIOS2_BUILTIN_CUSTOM_FN, &custom_fn, nios2_expand_custom_Xn},
    {CODE_FOR_custom_fni, "__builtin_custom_fni", NIOS2_BUILTIN_CUSTOM_FNI, &custom_fni, nios2_expand_custom_XnX},
    {CODE_FOR_custom_fnf, "__builtin_custom_fnf", NIOS2_BUILTIN_CUSTOM_FNF, &custom_fnf, nios2_expand_custom_XnX},
    {CODE_FOR_custom_fnp, "__builtin_custom_fnp", NIOS2_BUILTIN_CUSTOM_FNP, &custom_fnp, nios2_expand_custom_XnX},
    {CODE_FOR_custom_fnii, "__builtin_custom_fnii", NIOS2_BUILTIN_CUSTOM_FNII, &custom_fnii, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnif, "__builtin_custom_fnif", NIOS2_BUILTIN_CUSTOM_FNIF, &custom_fnif, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnip, "__builtin_custom_fnip", NIOS2_BUILTIN_CUSTOM_FNIP, &custom_fnip, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnfi, "__builtin_custom_fnfi", NIOS2_BUILTIN_CUSTOM_FNFI, &custom_fnfi, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnff, "__builtin_custom_fnff", NIOS2_BUILTIN_CUSTOM_FNFF, &custom_fnff, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnfp, "__builtin_custom_fnfp", NIOS2_BUILTIN_CUSTOM_FNFP, &custom_fnfp, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnpi, "__builtin_custom_fnpi", NIOS2_BUILTIN_CUSTOM_FNPI, &custom_fnpi, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnpf, "__builtin_custom_fnpf", NIOS2_BUILTIN_CUSTOM_FNPF, &custom_fnpf, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_fnpp, "__builtin_custom_fnpp", NIOS2_BUILTIN_CUSTOM_FNPP, &custom_fnpp, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pn, "__builtin_custom_pn", NIOS2_BUILTIN_CUSTOM_PN, &custom_pn, nios2_expand_custom_Xn},
    {CODE_FOR_custom_pni, "__builtin_custom_pni", NIOS2_BUILTIN_CUSTOM_PNI, &custom_pni, nios2_expand_custom_XnX},
    {CODE_FOR_custom_pnf, "__builtin_custom_pnf", NIOS2_BUILTIN_CUSTOM_PNF, &custom_pnf, nios2_expand_custom_XnX},
    {CODE_FOR_custom_pnp, "__builtin_custom_pnp", NIOS2_BUILTIN_CUSTOM_PNP, &custom_pnp, nios2_expand_custom_XnX},
    {CODE_FOR_custom_pnii, "__builtin_custom_pnii", NIOS2_BUILTIN_CUSTOM_PNII, &custom_pnii, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnif, "__builtin_custom_pnif", NIOS2_BUILTIN_CUSTOM_PNIF, &custom_pnif, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnip, "__builtin_custom_pnip", NIOS2_BUILTIN_CUSTOM_PNIP, &custom_pnip, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnfi, "__builtin_custom_pnfi", NIOS2_BUILTIN_CUSTOM_PNFI, &custom_pnfi, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnff, "__builtin_custom_pnff", NIOS2_BUILTIN_CUSTOM_PNFF, &custom_pnff, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnfp, "__builtin_custom_pnfp", NIOS2_BUILTIN_CUSTOM_PNFP, &custom_pnfp, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnpi, "__builtin_custom_pnpi", NIOS2_BUILTIN_CUSTOM_PNPI, &custom_pnpi, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnpf, "__builtin_custom_pnpf", NIOS2_BUILTIN_CUSTOM_PNPF, &custom_pnpf, nios2_expand_custom_XnXX},
    {CODE_FOR_custom_pnpp, "__builtin_custom_pnpp", NIOS2_BUILTIN_CUSTOM_PNPP, &custom_pnpp, nios2_expand_custom_XnXX},


    {0, 0, 0, 0, 0},
};

/* This does not have a closing bracket on purpose (see use) */
#define def_param(TYPE) \
  tree_cons (NULL_TREE, TYPE,

static void
nios2_init_builtins ()
{
  const struct builtin_description *d;


  endlink = void_list_node;

  /* Special indenting here because one of the brackets is in def_param */
  /* *INDENT-OFF* */

  /* int fn (volatile const void *)
   */
  int_ftype_volatile_const_void_p
    = build_function_type (integer_type_node,
			   def_param (build_qualified_type (ptr_type_node,
			                                    TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE))
			   endlink));


  /* void fn (volatile void *, int)
   */
  void_ftype_volatile_void_p_int
    = build_function_type (void_type_node,
			   def_param (build_qualified_type (ptr_type_node,
			                                    TYPE_QUAL_VOLATILE))
			   def_param (integer_type_node)
			   endlink)));

  /* void fn (void)
   */
  void_ftype_void
      = build_function_type (void_type_node,
                             endlink);

  /* int fn (int)
   */
  int_ftype_int
      = build_function_type (integer_type_node,
                             def_param (integer_type_node)
                             endlink));

  /* void fn (int, int)
   */
  void_ftype_int_int
      = build_function_type (void_type_node,
                             def_param (integer_type_node)
                             def_param (integer_type_node)
                             endlink)));


#define CUSTOM_NUM def_param (integer_type_node)

  custom_n
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     endlink));
  custom_ni
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     endlink)));
  custom_nf
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     endlink)));
  custom_np
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     endlink)));
  custom_nii
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_nif
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_nip
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_nfi
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_nff
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_nfp
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_npi
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_npf
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_npp
      = build_function_type (void_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));

  custom_in
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     endlink));
  custom_ini
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     endlink)));
  custom_inf
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     endlink)));
  custom_inp
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     endlink)));
  custom_inii
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_inif
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_inip
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_infi
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_inff
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_infp
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_inpi
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_inpf
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_inpp
      = build_function_type (integer_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));

  custom_fn
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     endlink));
  custom_fni
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     endlink)));
  custom_fnf
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     endlink)));
  custom_fnp
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     endlink)));
  custom_fnii
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_fnif
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_fnip
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_fnfi
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_fnff
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_fnfp
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_fnpi
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_fnpf
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_fnpp
      = build_function_type (float_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));


  custom_pn
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     endlink));
  custom_pni
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     endlink)));
  custom_pnf
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     endlink)));
  custom_pnp
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     endlink)));
  custom_pnii
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_pnif
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_pnip
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (integer_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_pnfi
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_pnff
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_pnfp
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (float_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));
  custom_pnpi
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (integer_type_node)
  			     endlink))));
  custom_pnpf
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (float_type_node)
  			     endlink))));
  custom_pnpp
      = build_function_type (ptr_type_node,
  			     CUSTOM_NUM
  			     def_param (ptr_type_node)
  			     def_param (ptr_type_node)
  			     endlink))));



  /* *INDENT-ON* */


  for (d = bdesc; d->name; d++)
    {
      builtin_function (d->name, *d->type, d->code,
			BUILT_IN_MD, NULL, NULL);
    }
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
nios2_expand_builtin (tree exp, rtx target, rtx subtarget, 
                      enum machine_mode mode, int ignore)
{
  const struct builtin_description *d;
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  for (d = bdesc; d->name; d++)
    if (d->code == fcode)
      return (d->expander) (d, exp, target, subtarget, mode, ignore);

  /* we should have seen one of the functins we registered */
  abort ();
}

static rtx nios2_create_target (const struct builtin_description *, rtx);


static rtx
nios2_create_target (const struct builtin_description *d, rtx target)
{
  if (!target
      || !(*insn_data[d->icode].operand[0].predicate) (target,
                                                       insn_data[d->icode].operand[0].mode))
    {
      target = gen_reg_rtx (insn_data[d->icode].operand[0].mode);
    }

  return target;
}


static rtx nios2_extract_opcode (const struct builtin_description *, int, tree);
static rtx nios2_extract_operand (const struct builtin_description *, int, int, tree);

static rtx
nios2_extract_opcode (const struct builtin_description *d, int op, tree arglist)
{
  enum machine_mode mode = insn_data[d->icode].operand[op].mode;
  tree arg = TREE_VALUE (arglist);
  rtx opcode = expand_expr (arg, NULL_RTX, mode, 0);
  opcode = protect_from_queue (opcode, 0);

  if (!(*insn_data[d->icode].operand[op].predicate) (opcode, mode))
    error ("Custom instruction opcode must be compile time constant in the range 0-255 for %s", d->name);

  return opcode;
}

static rtx
nios2_extract_operand (const struct builtin_description *d, int op, int argnum, tree arglist)
{
  enum machine_mode mode = insn_data[d->icode].operand[op].mode;
  tree arg = TREE_VALUE (arglist);
  rtx operand = expand_expr (arg, NULL_RTX, mode, 0);
  operand = protect_from_queue (operand, 0);

  if (!(*insn_data[d->icode].operand[op].predicate) (operand, mode))
    operand = copy_to_mode_reg (mode, operand);

  /* ??? Better errors would be nice */
  if (!(*insn_data[d->icode].operand[op].predicate) (operand, mode))
    error ("Invalid argument %d to %s", argnum, d->name);

  return operand;
}


static rtx
nios2_expand_custom_n (const struct builtin_description *d, tree exp, 
                       rtx target ATTRIBUTE_UNUSED, rtx subtarget ATTRIBUTE_UNUSED, 
                       enum machine_mode mode ATTRIBUTE_UNUSED, int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx opcode;

  /* custom_n should have exactly one operand */
  if (insn_data[d->icode].n_operands != 1)
    abort ();

  opcode = nios2_extract_opcode (d, 0, arglist);

  pat = GEN_FCN (d->icode) (opcode);
  if (!pat)
    return 0;
  emit_insn (pat);
  return 0;
}

static rtx
nios2_expand_custom_Xn (const struct builtin_description *d, tree exp, 
                        rtx target, rtx subtarget ATTRIBUTE_UNUSED, 
                        enum machine_mode mode ATTRIBUTE_UNUSED, 
                        int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx opcode;

  /* custom_Xn should have exactly two operands */
  if (insn_data[d->icode].n_operands != 2)
    abort ();

  target = nios2_create_target (d, target);
  opcode = nios2_extract_opcode (d, 1, arglist);

  pat = GEN_FCN (d->icode) (target, opcode);
  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}

static rtx
nios2_expand_custom_nX (const struct builtin_description *d, tree exp, 
                        rtx target ATTRIBUTE_UNUSED, rtx subtarget ATTRIBUTE_UNUSED, 
                        enum machine_mode mode ATTRIBUTE_UNUSED, int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx opcode;
  rtx operands[1];
  int i;


  /* custom_nX should have exactly two operands */
  if (insn_data[d->icode].n_operands != 2)
    abort ();

  opcode = nios2_extract_opcode (d, 0, arglist);
  for (i = 0; i < 1; i++)
    {
      arglist = TREE_CHAIN (arglist);
      operands[i] = nios2_extract_operand (d, i + 1, i + 1, arglist);
    }

  pat = GEN_FCN (d->icode) (opcode, operands[0]);
  if (!pat)
    return 0;
  emit_insn (pat);
  return 0;
}

static rtx
nios2_expand_custom_XnX (const struct builtin_description *d, tree exp, rtx target, 
                         rtx subtarget ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED, 
                         int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx opcode;
  rtx operands[1];
  int i;

  /* custom_Xn should have exactly three operands */
  if (insn_data[d->icode].n_operands != 3)
    abort ();

  target = nios2_create_target (d, target);
  opcode = nios2_extract_opcode (d, 1, arglist);

  for (i = 0; i < 1; i++)
    {
      arglist = TREE_CHAIN (arglist);
      operands[i] = nios2_extract_operand (d, i + 2, i + 1, arglist);
    }

  pat = GEN_FCN (d->icode) (target, opcode, operands[0]);

  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}

static rtx
nios2_expand_custom_nXX (const struct builtin_description *d, tree exp, rtx target ATTRIBUTE_UNUSED, 
                         rtx subtarget ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED, 
                         int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx opcode;
  rtx operands[2];
  int i;


  /* custom_nX should have exactly three operands */
  if (insn_data[d->icode].n_operands != 3)
    abort ();

  opcode = nios2_extract_opcode (d, 0, arglist);
  for (i = 0; i < 2; i++)
    {
      arglist = TREE_CHAIN (arglist);
      operands[i] = nios2_extract_operand (d, i + 1, i + 1, arglist);
    }

  pat = GEN_FCN (d->icode) (opcode, operands[0], operands[1]);
  if (!pat)
    return 0;
  emit_insn (pat);
  return 0;
}

static rtx
nios2_expand_custom_XnXX (const struct builtin_description *d, tree exp, rtx target, 
                          rtx subtarget ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED, 
                          int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx opcode;
  rtx operands[2];
  int i;


  /* custom_XnX should have exactly four operands */
  if (insn_data[d->icode].n_operands != 4)
    abort ();

  target = nios2_create_target (d, target);
  opcode = nios2_extract_opcode (d, 1, arglist);
  for (i = 0; i < 2; i++)
    {
      arglist = TREE_CHAIN (arglist);
      operands[i] = nios2_extract_operand (d, i + 2, i + 1, arglist);
    }

  pat = GEN_FCN (d->icode) (target, opcode, operands[0], operands[1]);

  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}



static rtx
nios2_expand_STXIO (const struct builtin_description *d, tree exp, rtx target ATTRIBUTE_UNUSED, 
                    rtx subtarget ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED, 
                    int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx store_dest, store_val;
  enum insn_code icode = d->icode;

  /* stores should have exactly two operands */
  if (insn_data[icode].n_operands != 2)
    abort ();

  /* process the destination of the store */
  {
    enum machine_mode mode = insn_data[icode].operand[0].mode;
    tree arg = TREE_VALUE (arglist);
    store_dest = expand_expr (arg, NULL_RTX, VOIDmode, 0);
    store_dest = protect_from_queue (store_dest, 0);

    store_dest = gen_rtx_MEM (mode, copy_to_mode_reg (Pmode, store_dest));

    /* ??? Better errors would be nice */
    if (!(*insn_data[icode].operand[0].predicate) (store_dest, mode))
      error ("Invalid argument 1 to %s", d->name);
  }


  /* process the value to store */
  {
    enum machine_mode mode = insn_data[icode].operand[1].mode;
    tree arg = TREE_VALUE (TREE_CHAIN (arglist));
    store_val = expand_expr (arg, NULL_RTX, mode, 0);
    store_val = protect_from_queue (store_val, 0);

    if (!(*insn_data[icode].operand[1].predicate) (store_val, mode))
      store_val = copy_to_mode_reg (mode, store_val);

    /* ??? Better errors would be nice */
    if (!(*insn_data[icode].operand[1].predicate) (store_val, mode))
      error ("Invalid argument 2 to %s", d->name);
  }

  pat = GEN_FCN (d->icode) (store_dest, store_val);
  if (!pat)
    return 0;
  emit_insn (pat);
  return 0;
}


static rtx
nios2_expand_LDXIO (const struct builtin_description * d, tree exp, rtx target, 
                    rtx subtarget ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED, 
                    int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx ld_src;
  enum insn_code icode = d->icode;

  /* loads should have exactly two operands */
  if (insn_data[icode].n_operands != 2)
    abort ();

  target = nios2_create_target (d, target);

  {
    enum machine_mode mode = insn_data[icode].operand[1].mode;
    tree arg = TREE_VALUE (arglist);
    ld_src = expand_expr (arg, NULL_RTX, VOIDmode, 0);
    ld_src = protect_from_queue (ld_src, 0);

    ld_src = gen_rtx_MEM (mode, copy_to_mode_reg (Pmode, ld_src));

    /* ??? Better errors would be nice */
    if (!(*insn_data[icode].operand[1].predicate) (ld_src, mode))
      {
        error ("Invalid argument 1 to %s", d->name);
      }
  }

  pat = GEN_FCN (d->icode) (target, ld_src);
  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}


static rtx
nios2_expand_sync (const struct builtin_description * d ATTRIBUTE_UNUSED, 
                   tree exp ATTRIBUTE_UNUSED, rtx target ATTRIBUTE_UNUSED, 
                   rtx subtarget ATTRIBUTE_UNUSED, 
                   enum machine_mode mode ATTRIBUTE_UNUSED, 
                   int ignore ATTRIBUTE_UNUSED)
{
  emit_insn (gen_sync ());
  return 0;
}

static rtx
nios2_expand_rdctl (const struct builtin_description * d ATTRIBUTE_UNUSED, 
                   tree exp ATTRIBUTE_UNUSED, rtx target ATTRIBUTE_UNUSED, 
                   rtx subtarget ATTRIBUTE_UNUSED, 
                   enum machine_mode mode ATTRIBUTE_UNUSED, 
                   int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx rdctl_reg;
  enum insn_code icode = d->icode;

  /* rdctl should have exactly two operands */
  if (insn_data[icode].n_operands != 2)
    abort ();

  target = nios2_create_target (d, target);

  {
    enum machine_mode mode = insn_data[icode].operand[1].mode;
    tree arg = TREE_VALUE (arglist);
    rdctl_reg = expand_expr (arg, NULL_RTX, VOIDmode, 0);
    rdctl_reg = protect_from_queue (rdctl_reg, 0);

    if (!(*insn_data[icode].operand[1].predicate) (rdctl_reg, mode))
      {
        error ("Control register number must be in range 0-31 for %s", d->name);
      }
  }

  pat = GEN_FCN (d->icode) (target, rdctl_reg);
  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}

static rtx
nios2_expand_wrctl (const struct builtin_description * d ATTRIBUTE_UNUSED, 
                   tree exp ATTRIBUTE_UNUSED, rtx target ATTRIBUTE_UNUSED, 
                   rtx subtarget ATTRIBUTE_UNUSED, 
                   enum machine_mode mode ATTRIBUTE_UNUSED, 
                   int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx pat;
  rtx wrctl_reg, store_val;
  enum insn_code icode = d->icode;

  /* stores should have exactly two operands */
  if (insn_data[icode].n_operands != 2)
    abort ();

  /* process the destination of the store */
  {
    enum machine_mode mode = insn_data[icode].operand[0].mode;
    tree arg = TREE_VALUE (arglist);
    wrctl_reg = expand_expr (arg, NULL_RTX, VOIDmode, 0);
    wrctl_reg = protect_from_queue (wrctl_reg, 0);

    if (!(*insn_data[icode].operand[0].predicate) (wrctl_reg, mode))
      error ("Control register number must be in range 0-31 for %s", d->name);
  }


  /* process the value to store */
  {
    enum machine_mode mode = insn_data[icode].operand[1].mode;
    tree arg = TREE_VALUE (TREE_CHAIN (arglist));
    store_val = expand_expr (arg, NULL_RTX, mode, 0);
    store_val = protect_from_queue (store_val, 0);

    if (!(*insn_data[icode].operand[1].predicate) (store_val, mode))
      store_val = copy_to_mode_reg (mode, store_val);

    /* ??? Better errors would be nice */
    if (!(*insn_data[icode].operand[1].predicate) (store_val, mode))
      error ("Invalid argument 2 to %s", d->name);
  }

  pat = GEN_FCN (d->icode) (wrctl_reg, store_val);
  if (!pat)
    return 0;
  emit_insn (pat);
  return 0;
}


#include "gt-nios2.h"

