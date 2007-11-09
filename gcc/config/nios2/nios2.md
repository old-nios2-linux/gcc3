;; Machine Description for Altera NIOS 2G NIOS2 version.
;;    Copyright (C) 2003 Altera 
;;    Contributed by Jonah Graham (jgraham@altera.com).
;; 
;; This file is part of GNU CC.
;; 
;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.  */



;*****************************************************************************
;*
;* constants
;*
;*****************************************************************************
(define_constants [
  (GP_REGNO 26)
  (SP_REGNO 27)
  (FP_REGNO 28)
  (RA_REGNO 31)
  (RAP_REGNO 38)
  (FIRST_RETVAL_REGNO 2)
  (LAST_RETVAL_REGNO 3)
  (FIRST_ARG_REGNO 4)
  (LAST_ARG_REGNO 7)
  (SC_REGNO 23)
  (PC_REGNO 37)
  (FAKE_FP_REGNO 39)
  (FAKE_AP_REGNO 40)


  (UNSPEC_BLOCKAGE 0)
  (UNSPEC_LDBIO 1)
  (UNSPEC_LDBUIO 2)
  (UNSPEC_LDHIO 3)
  (UNSPEC_LDHUIO 4)
  (UNSPEC_LDWIO 5)
  (UNSPEC_STBIO 6)
  (UNSPEC_STHIO 7)
  (UNSPEC_STWIO 8)
  (UNSPEC_SYNC 9)
  (UNSPEC_WRCTL 10)
  (UNSPEC_RDCTL 11)
  
])



;*****************************************************************************
;*
;* instruction scheduler
;*
;*****************************************************************************

; No schedule info is currently available, using an assumption that no
; instruction can use the results of the previous instruction without
; incuring a stall.

; length of an instruction (in bytes)
(define_attr "length" "" (const_int 4))
(define_attr "type" "unknown,complex,control,alu,cond_alu,st,ld,shift,mul,div,custom" (const_string "complex"))

(define_asm_attributes
 [(set_attr "length" "4")
  (set_attr "type" "complex")])

(define_automaton "nios2")
(automata_option "v")
;(automata_option "no-minimization")
(automata_option "ndfa")

; The nios2 pipeline is fairly straightforward for the fast model.
; Every alu operation is pipelined so that an instruction can
; be issued every cycle. However, there are still potential
; stalls which this description tries to deal with.

(define_cpu_unit "cpu" "nios2")

(define_insn_reservation "complex" 1
  (eq_attr "type" "complex")
  "cpu")

(define_insn_reservation "control" 1
  (eq_attr "type" "control")
  "cpu")

(define_insn_reservation "alu" 1
  (eq_attr "type" "alu")
  "cpu")

(define_insn_reservation "cond_alu" 1
  (eq_attr "type" "cond_alu")
  "cpu")

(define_insn_reservation "st" 1
  (eq_attr "type" "st")
  "cpu")
  
(define_insn_reservation "custom" 1
  (eq_attr "type" "custom")
  "cpu")

; shifts, muls and lds have three cycle latency
(define_insn_reservation "ld" 3
  (eq_attr "type" "ld")
  "cpu")

(define_insn_reservation "shift" 3
  (eq_attr "type" "shift")
  "cpu")

(define_insn_reservation "mul" 3
  (eq_attr "type" "mul")
  "cpu")

(define_insn_reservation "div" 1
  (eq_attr "type" "div")
  "cpu")


;*****************************************************************************
;*
;* MOV Instructions
;*
;*****************************************************************************

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
{
  if (nios2_emit_move_sequence (operands, QImode))
    DONE;
})

(define_insn "movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=m, r,r, r")
        (match_operand:QI 1 "general_operand"       "rM,m,rM,I"))]
  "(register_operand (operands[0], QImode)
    || register_operand (operands[1], QImode)
    || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "@
    stb%o0\\t%z1, %0
    ldbu%o1\\t%0, %1
    mov\\t%0, %z1
    movi\\t%0, %1"
  [(set_attr "type" "st,ld,alu,alu")])

(define_insn "ldbio"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_LDBIO))
   (use (match_operand:SI 1 "memory_operand" "m"))]
  ""
  "ldbio\\t%0, %1"
  [(set_attr "type" "ld")])

(define_insn "ldbuio"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_LDBUIO))
   (use (match_operand:SI 1 "memory_operand" "m"))]
  ""
  "ldbuio\\t%0, %1"
  [(set_attr "type" "ld")])

(define_insn "stbio"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "register_operand"   "r"))
   (unspec_volatile:SI [(const_int 0)] UNSPEC_STBIO)]
  ""
  "stbio\\t%z1, %0"
  [(set_attr "type" "st")])


(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
{
  if (nios2_emit_move_sequence (operands, HImode))
    DONE;
})

(define_insn "movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=m, r,r, r,r")
        (match_operand:HI 1 "general_operand"       "rM,m,rM,I,J"))]
  "(register_operand (operands[0], HImode)
    || register_operand (operands[1], HImode)
    || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "@
    sth%o0\\t%z1, %0
    ldhu%o1\\t%0, %1
    mov\\t%0, %z1
    movi\\t%0, %1
    movui\\t%0, %1"
  [(set_attr "type" "st,ld,alu,alu,alu")])

(define_insn "ldhio"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_LDHIO))
   (use (match_operand:SI 1 "memory_operand" "m"))]
  ""
  "ldhio\\t%0, %1"
  [(set_attr "type" "ld")])

(define_insn "ldhuio"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_LDHUIO))
   (use (match_operand:SI 1 "memory_operand" "m"))]
  ""
  "ldhuio\\t%0, %1"
  [(set_attr "type" "ld")])

(define_insn "sthio"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "register_operand"   "r"))
   (unspec_volatile:SI [(const_int 0)] UNSPEC_STHIO)]
  ""
  "sthio\\t%z1, %0"
  [(set_attr "type" "st")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
{
  if (nios2_emit_move_sequence (operands, SImode))
    DONE;
})

(define_insn "movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=m, r,r, r,r,r,r")
        (match_operand:SI 1 "general_operand"       "rM,m,rM,I,J,S,i"))]
  "(register_operand (operands[0], SImode)
    || register_operand (operands[1], SImode)
    || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "@
    stw%o0\\t%z1, %0
    ldw%o1\\t%0, %1
    mov\\t%0, %z1
    movi\\t%0, %1
    movui\\t%0, %1
    addi\\t%0, gp, %%gprel(%1)
    movhi\\t%0, %H1\;addi\\t%0, %0, %L1"
  [(set_attr "type" "st,ld,alu,alu,alu,alu,alu")])

(define_insn "ldwio"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_LDWIO))
   (use (match_operand:SI 1 "memory_operand" "m"))]
  ""
  "ldwio\\t%0, %1"
  [(set_attr "type" "ld")])

(define_insn "stwio"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "register_operand"   "r"))
   (unspec_volatile:SI [(const_int 0)] UNSPEC_STWIO)]
  ""
  "stwio\\t%z1, %0"
  [(set_attr "type" "st")])



;*****************************************************************************
;*
;* zero extension
;*
;*****************************************************************************


(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
    andi\\t%0, %1, 0xffff
    ldhu%o1\\t%0, %1"
  [(set_attr "type"	"alu,ld")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
    andi\\t%0, %1, 0xff
    ldbu%o1\\t%0, %1"
  [(set_attr "type"	"alu,ld")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
    andi\\t%0, %1, 0xff
    ldbu%o1\\t%0, %1"
  [(set_attr "type"	"alu,ld")])



;*****************************************************************************
;*
;* sign extension
;*
;*****************************************************************************

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  ""
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (SImode, operands[1]);
      rtx temp  = gen_reg_rtx (SImode);
      rtx shift = GEN_INT (16);

      emit_insn (gen_ashlsi3 (temp, op1, shift));
      emit_insn (gen_ashrsi3 (operands[0], temp, shift));
      DONE;
    }
})

(define_insn "extendhisi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "ldh%o1\\t%0, %1"
  [(set_attr "type"	"ld")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op0   = gen_lowpart (SImode, operands[0]);
      rtx op1   = gen_lowpart (SImode, operands[1]);
      rtx temp  = gen_reg_rtx (SImode);
      rtx shift = GEN_INT (24);

      emit_insn (gen_ashlsi3 (temp, op1, shift));
      emit_insn (gen_ashrsi3 (op0, temp, shift));
      DONE;
    }
})

(define_insn "extendqihi2_internal"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldb%o1\\t%0, %1"
  [(set_attr "type"	"ld")])


(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (SImode, operands[1]);
      rtx temp  = gen_reg_rtx (SImode);
      rtx shift = GEN_INT (24);

      emit_insn (gen_ashlsi3 (temp, op1, shift));
      emit_insn (gen_ashrsi3 (operands[0], temp, shift));
      DONE;
    }
})

(define_insn "extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldb%o1\\t%0, %1"
  [(set_attr "type"	"ld")])



;*****************************************************************************
;*
;* Arithmetic Operations
;*
;*****************************************************************************

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r")
        (plus:SI (match_operand:SI 1 "register_operand" "%r,r")
                 (match_operand:SI 2 "arith_operand"     "r,I")))]
  ""
  "add%i2\\t%0, %1, %z2"
  [(set_attr "type" "alu")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (minus:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
                  (match_operand:SI 2 "register_operand"  "r")))]
  ""
  "sub\\t%0, %z1, %2"
  [(set_attr "type" "alu")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r,r")
        (mult:SI (match_operand:SI 1 "register_operand"    "r,r")
                 (match_operand:SI 2 "arith_operand"       "r,I")))]
  "TARGET_HAS_MUL"
  "mul%i2\\t%0, %1, %z2"
  [(set_attr "type" "mul")])

(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r")
        (div:SI (match_operand:SI 1 "register_operand"     "r")
                (match_operand:SI 2 "register_operand"     "r")))]
  ""
{
  if (!TARGET_HAS_DIV)
    {
      if (!TARGET_FAST_SW_DIV)
	FAIL;
      else
        {
	  if (nios2_emit_expensive_div (operands, SImode))
	    DONE;
	}
    }
})

(define_insn "divsi3_insn"
  [(set (match_operand:SI 0 "register_operand"            "=r")
        (div:SI (match_operand:SI 1 "register_operand"     "r")
                (match_operand:SI 2 "register_operand"     "r")))]
  "TARGET_HAS_DIV"
  "div\\t%0, %1, %2"
  [(set_attr "type" "div")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r")
        (udiv:SI (match_operand:SI 1 "register_operand"     "r")
                (match_operand:SI 2 "register_operand"     "r")))]
  "TARGET_HAS_DIV"
  "divu\\t%0, %1, %2"
  [(set_attr "type" "div")])

(define_insn "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                            "=r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"  "r"))
		   (sign_extend:DI (match_operand:SI 2 "register_operand"  "r")))
	  (const_int 32))))]
  "TARGET_HAS_MULX"
  "mulxss\\t%0, %1, %2"
  [(set_attr "type" "mul")])

(define_insn "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                            "=r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"  "r"))
		   (zero_extend:DI (match_operand:SI 2 "register_operand"  "r")))
	  (const_int 32))))]
  "TARGET_HAS_MULX"
  "mulxuu\\t%0, %1, %2"
  [(set_attr "type" "mul")])


(define_expand "mulsidi3"
    [(set (subreg:SI (match_operand:DI 0 "register_operand" "") 0)
	  (mult:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "register_operand" "")))
     (set (subreg:SI (match_dup 0) 4)
	  (truncate:SI (lshiftrt:DI (mult:DI (sign_extend:DI (match_dup 1))
					     (sign_extend:DI (match_dup 2)))
				    (const_int 32))))]
  "TARGET_HAS_MULX"
  "")

(define_expand "umulsidi3"
    [(set (subreg:SI (match_operand:DI 0 "register_operand" "") 0)
	  (mult:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "register_operand" "")))
     (set (subreg:SI (match_dup 0) 4)
	  (truncate:SI (lshiftrt:DI (mult:DI (zero_extend:DI (match_dup 1))
					     (zero_extend:DI (match_dup 2)))
				    (const_int 32))))]
  "TARGET_HAS_MULX"
  "")



;*****************************************************************************
;*
;* Negate and ones complement
;*
;*****************************************************************************

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
{
  operands[2] = const0_rtx;
  return "sub\\t%0, %z2, %1";
}
  [(set_attr "type" "alu")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
{
  operands[2] = const0_rtx;
  return "nor\\t%0, %z2, %1";
}
  [(set_attr "type" "alu")])



; Logical Operantions

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r, r,r")
        (and:SI (match_operand:SI 1 "register_operand" "%r, r,r")
                (match_operand:SI 2 "logical_operand"   "rM,J,K")))]
  ""
  "@
    and\\t%0, %1, %z2
    and%i2\\t%0, %1, %2
    andh%i2\\t%0, %1, %U2"
  [(set_attr "type" "alu")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r, r,r")
        (ior:SI (match_operand:SI 1 "register_operand"  "%r, r,r")
                (match_operand:SI 2 "logical_operand"    "rM,J,K")))]
  ""
  "@
    or\\t%0, %1, %z2
    or%i2\\t%0, %1, %2
    orh%i2\\t%0, %1, %U2"
  [(set_attr "type" "alu")])

(define_insn "*norsi3"
  [(set (match_operand:SI 0 "register_operand"                  "=r")
        (and:SI (not:SI (match_operand:SI 1 "register_operand"  "%r"))
                (not:SI (match_operand:SI 2 "reg_or_0_operand"   "rM"))))]
  ""
  "nor\\t%0, %1, %z2"
  [(set_attr "type" "alu")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r, r,r")
        (xor:SI (match_operand:SI 1 "register_operand"  "%r, r,r")
                (match_operand:SI 2 "logical_operand"    "rM,J,K")))]
  ""
  "@
    xor\\t%0, %1, %z2
    xor%i2\\t%0, %1, %2
    xorh%i2\\t%0, %1, %U2"
  [(set_attr "type" "alu")])



;*****************************************************************************
;*
;* Shifts
;*
;*****************************************************************************

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"           "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r,r")
		   (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "sll%i2\\t%0, %1, %z2"
  [(set_attr "type" "shift")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"             "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "sra%i2\\t%0, %1, %z2"
  [(set_attr "type" "shift")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"             "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "srl%i2\\t%0, %1, %z2"
  [(set_attr "type" "shift")])

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "register_operand"           "=r,r")
	(rotate:SI (match_operand:SI 1 "register_operand" "r,r")
		   (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "rol%i2\\t%0, %1, %z2"
  [(set_attr "type" "shift")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand"             "=r,r")
	(rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "register_operand" "r,r")))]
  ""
  "ror\\t%0, %1, %2"
  [(set_attr "type" "shift")])

(define_insn "*shift_mul_constants"
  [(set (match_operand:SI 0 "register_operand"                     "=r")
	(ashift:SI (mult:SI (match_operand:SI 1 "register_operand"  "r")
		            (match_operand:SI 2 "const_int_operand" "I"))
		   (match_operand:SI 3          "const_int_operand" "I")))]
  "TARGET_HAS_MUL && SMALL_INT (INTVAL (operands[2]) << INTVAL (operands[3]))"
{
  HOST_WIDE_INT mul = INTVAL (operands[2]) << INTVAL (operands[3]);
  rtx ops[3];
  
  ops[0] = operands[0];
  ops[1] = operands[1];
  ops[2] = GEN_INT (mul);
  
  output_asm_insn ("muli\t%0, %1, %2", ops);
  return "";
}
  [(set_attr "type" "mul")])




;*****************************************************************************
;*
;* Prologue, Epilogue and Return
;*
;*****************************************************************************

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(return)]
  ""
{
  expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  expand_epilogue (true);
  DONE;
})

(define_insn "return"
  [(return)]
  "reload_completed && nios2_can_use_return_insn ()"
  "ret\\t"
)

(define_insn "return_from_epilogue"
  [(use (match_operand 0 "pmode_register_operand" ""))
   (return)]
  "reload_completed"
  "ret\\t"
)

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "unknown")
   (set_attr "length" "0")])



;*****************************************************************************
;*
;* Jumps and Calls
;*
;*****************************************************************************

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp\\t%0"
  [(set_attr "type" "control")])

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "br\\t%0"
  [(set_attr "type" "control")])


(define_insn "indirect_call"
  [(call (mem:QI (match_operand:SI 0 "register_operand" "r"))
         (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNO))]
  ""
  "callr\\t%0"
  [(set_attr "type" "control")])

(define_insn "indirect_call_value"
  [(set (match_operand 0 "" "")
        (call (mem:QI (match_operand:SI 1 "register_operand" "r"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RA_REGNO))]
  ""
  "callr\\t%1"
)

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "")
                    (match_operand 1 "" ""))
              (clobber (reg:SI RA_REGNO))])]
  ""
  "")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
                   (call (match_operand 1 "" "")
                         (match_operand 2 "" "")))
              (clobber (reg:SI RA_REGNO))])]
  ""
  "")

(define_insn "*call"
  [(call (mem:QI (match_operand:SI 0 "immediate_operand" "i"))
         (match_operand 1 "" ""))
   (clobber (match_operand:SI 2 "register_operand" "=r"))]
  ""
  "call\\t%0"
  [(set_attr "type" "control")])

(define_insn "*call_value"
  [(set (match_operand 0 "" "")
        (call (mem:QI (match_operand:SI 1 "immediate_operand" "i"))
              (match_operand 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" "=r"))]
  ""
  "call\\t%1"
  [(set_attr "type" "control")])

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (return)
	      (use (match_operand 2 "" ""))])]
  ""
  {
    XEXP (operands[0], 0) = copy_to_mode_reg (SImode, XEXP (operands[0], 0));

    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
  }
)

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (return)
	      (use (match_operand 3 "" ""))])]
  ""
  {
    XEXP (operands[1], 0) = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

    if (operands[3] == NULL_RTX)
      operands[3] = const0_rtx;
  }
)

(define_insn "sibcall_insn"
 [(call (mem:QI (match_operand:SI 0 "register_operand" "r"))
	(match_operand 1 "" ""))
  (return)
  (use (match_operand 2 "" ""))]
  ""
  "jmp\\t%0"
)

(define_insn "sibcall_value_insn"
 [(set (match_operand 0 "register_operand" "")
       (call (mem:QI (match_operand:SI 1 "register_operand" "r"))
	     (match_operand 2 "" "")))
  (return)
  (use (match_operand 3 "" ""))]
  ""
  "jmp\\t%1"
)




(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand 0 "register_operand" "r"))
              (use (label_ref (match_operand 1 "" "")))])]
  ""
  ""
)

(define_insn "*tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp\\t%0"
  [(set_attr "type" "control")])



;*****************************************************************************
;*
;* Comparisons
;*
;*****************************************************************************
;; Flow here is rather complex (based on MIPS):
;;
;;  1)	The cmp{si,di,sf,df} routine is called.  It deposits the
;;	arguments into the branch_cmp array, and the type into
;;	branch_type.  No RTL is generated.
;;
;;  2)	The appropriate branch define_expand is called, which then
;;	creates the appropriate RTL for the comparison and branch.
;;	Different CC modes are used, based on what type of branch is
;;	done, so that we can constrain things appropriately.  There
;;	are assumptions in the rest of GCC that break if we fold the
;;	operands into the branchs for integer operations, and use cc0
;;	for floating point, so we use the fp status register instead.
;;	If needed, an appropriate temporary is created to hold the
;;	of the integer compare.

(define_expand "cmpsi"
  [(set (cc0)
	(compare:CC (match_operand:SI 0 "register_operand" "")
		    (match_operand:SI 1 "arith_operand" "")))]
  ""
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = operands[1];
  branch_type = CMP_SI;
  DONE;
})

(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" ""))]
  ""
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = const0_rtx;
  branch_type = CMP_SI;
  DONE;
})


;*****************************************************************************
;*
;* setting a register from a comparison
;*
;*****************************************************************************

(define_expand "seq"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (EQ, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*seq"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(eq:SI (match_operand:SI 1 "reg_or_0_operand" "%rM")
	       (match_operand:SI 2 "arith_operand"     "rI")))]
  ""
  "cmpeq%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])


(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (NE, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sne"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(ne:SI (match_operand:SI 1 "reg_or_0_operand" "%rM")
	       (match_operand:SI 2 "arith_operand"     "rI")))]
  ""
  "cmpne%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])


(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(gt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (GT, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sgt"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(gt:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	       (match_operand:SI 2 "reg_or_0_operand"  "rM")))]
  ""
  "cmplt\\t%0, %z2, %z1"
  [(set_attr "type" "alu")])


(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ge:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (GE, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sge"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(ge:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	       (match_operand:SI 2 "arith_operand"     "rI")))]
  ""
  "cmpge%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(le:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (LE, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sle"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(le:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	       (match_operand:SI 2 "reg_or_0_operand"  "rM")))]
  ""
  "cmpge\\t%0, %z2, %z1"
  [(set_attr "type" "alu")])


(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (LT, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*slt"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(lt:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	       (match_operand:SI 2 "arith_operand"     "rI")))]
  ""
  "cmplt%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])


(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(gtu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (GTU, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sgtu"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(gtu:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	        (match_operand:SI 2 "reg_or_0_operand"  "rM")))]
  ""
  "cmpltu\\t%0, %z2, %z1"
  [(set_attr "type" "alu")])


(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (GEU, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sgeu"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(geu:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	        (match_operand:SI 2 "uns_arith_operand"     "rJ")))]
  ""
  "cmpgeu%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(leu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (LEU, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sleu"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(leu:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	        (match_operand:SI 2 "reg_or_0_operand"  "rM")))]
  ""
  "cmpgeu\\t%0, %z2, %z1"
  [(set_attr "type" "alu")])


(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  gen_int_relational (LTU, operands[0], operands[1], operands[2], NULL_RTX);
  DONE;
})


(define_insn "*sltu"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(ltu:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
	        (match_operand:SI 2 "uns_arith_operand"     "rJ")))]
  ""
  "cmpltu%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])




;*****************************************************************************
;*
;* branches
;*
;*****************************************************************************

(define_insn "*cbranch"
  [(set (pc)
	(if_then_else
         (match_operator:SI 0 "comparison_operator"
			    [(match_operand:SI 2 "reg_or_0_operand" "rM")
			     (match_operand:SI 3 "reg_or_0_operand" "rM")])
        (label_ref (match_operand 1 "" ""))
        (pc)))]
  ""
  "b%0\\t%z2, %z3, %l1"
  [(set_attr "type" "control")])


(define_expand "beq"
  [(set (pc)
	(if_then_else (eq:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (EQ, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})


(define_expand "bne"
  [(set (pc)
	(if_then_else (ne:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (NE, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})


(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (GT, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (GE, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})

(define_expand "ble"
  [(set (pc)
	(if_then_else (le:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (LE, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (LT, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})


(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu:CC (cc0)
		 	      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (GTU, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (GEU, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (LEU, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_int_relational (LTU, NULL_RTX, branch_cmp[0], branch_cmp[1], operands[0]);
  DONE;
})


;*****************************************************************************
;*
;* String and Block Operations
;*
;*****************************************************************************

; ??? This is all really a hack to get Dhrystone to work as fast as possible
;     things to be fixed:
;        * let the compiler core handle all of this, for that to work the extra
;          aliasing needs to be addressed.
;        * we use three temporary registers for loading and storing to ensure no
;          ld use stalls, this is excessive, because after the first ld/st only
;          two are needed. Only two would be needed all the way through if 
;          we could schedule with other code. Consider:
;           1  ld $1, 0($src)
;           2  ld $2, 4($src)
;           3  ld $3, 8($src)
;           4  st $1, 0($dest)
;           5  ld $1, 12($src)
;           6  st $2, 4($src)
;           7  etc.
;          The first store has to wait until 4. If it does not there will be one
;          cycle of stalling. However, if any other instruction could be placed
;          between 1 and 4, $3 would not be needed.
;        * In small we probably don't want to ever do this ourself because there
;          is no ld use stall.

(define_expand "movstrsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand"  "")
		   (match_operand:BLK 1 "general_operand"  ""))
	      (use (match_operand:SI 2 "const_int_operand" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))
	      (clobber (match_scratch:SI 4                "=&r"))
	      (clobber (match_scratch:SI 5                "=&r"))
	      (clobber (match_scratch:SI 6                "=&r"))])]
  "TARGET_INLINE_MEMCPY"
{
  rtx ld_addr_reg, st_addr_reg;

  /* If the predicate for op2 fails in expr.c:emit_block_move_via_movstr 
     it trys to copy to a register, but does not re-try the predicate.
     ??? Intead of fixing expr.c, I fix it here. */
  if (!const_int_operand (operands[2], SImode))
    FAIL;

  /* ??? there are some magic numbers which need to be sorted out here.
         the basis for them is not increasing code size hugely or going
         out of range of offset addressing */
  if (INTVAL (operands[3]) < 4)
    FAIL;
  if (!optimize
      || (optimize_size && INTVAL (operands[2]) > 12)
      || (optimize < 3 && INTVAL (operands[2]) > 100)
      || INTVAL (operands[2]) > 200)
    FAIL;

  st_addr_reg
    = replace_equiv_address (operands[0],
			     copy_to_mode_reg (Pmode, XEXP (operands[0], 0)));
  ld_addr_reg
    = replace_equiv_address (operands[1],
			     copy_to_mode_reg (Pmode, XEXP (operands[1], 0)));
  emit_insn (gen_movstrsi_internal (st_addr_reg, ld_addr_reg,
				    operands[2], operands[3]));

  DONE;
})


(define_insn "movstrsi_internal"
  [(set (match_operand:BLK 0 "memory_operand"   "=o")
	(match_operand:BLK 1 "memory_operand"    "o"))
   (use (match_operand:SI 2 "const_int_operand"  "i"))
   (use (match_operand:SI 3 "const_int_operand"  "i"))
   (clobber (match_scratch:SI 4                "=&r"))
   (clobber (match_scratch:SI 5                "=&r"))
   (clobber (match_scratch:SI 6                "=&r"))]
  "TARGET_INLINE_MEMCPY"
{
  int ld_offset = INTVAL (operands[2]);
  int ld_len = INTVAL (operands[2]);
  int ld_reg = 0;
  rtx ld_addr_reg = XEXP (operands[1], 0);
  int st_offset = INTVAL (operands[2]);
  int st_len = INTVAL (operands[2]);
  int st_reg = 0;
  rtx st_addr_reg = XEXP (operands[0], 0);
  int delay_count = 0;
  
  /* ops[0] is the address used by the insn
     ops[1] is the register being loaded or stored */
  rtx ops[2];
  
  if (INTVAL (operands[3]) < 4)
    abort ();
  
  while (ld_offset >= 4)
    {
      /* if the load use delay has been met, I can start
         storing */
      if (delay_count >= 3)
        {
	  ops[0] = gen_rtx (MEM, SImode, 
			    plus_constant (st_addr_reg, st_len - st_offset));
	  ops[1] = operands[st_reg + 4];			 
	  output_asm_insn ("stw\t%1, %0", ops);
	  
	  st_reg = (st_reg + 1) % 3;
	  st_offset -= 4;
        }
    
      ops[0] = gen_rtx (MEM, SImode, 
			plus_constant (ld_addr_reg, ld_len - ld_offset));
      ops[1] = operands[ld_reg + 4];			 
      output_asm_insn ("ldw\t%1, %0", ops);
      
      ld_reg = (ld_reg + 1) % 3;
      ld_offset -= 4;
      delay_count++;
    }
  
  if (ld_offset >= 2)
    {
      /* if the load use delay has been met, I can start
         storing */
      if (delay_count >= 3)
        {
	  ops[0] = gen_rtx (MEM, SImode, 
			    plus_constant (st_addr_reg, st_len - st_offset));
	  ops[1] = operands[st_reg + 4];			 
	  output_asm_insn ("stw\t%1, %0", ops);
	  
	  st_reg = (st_reg + 1) % 3;
	  st_offset -= 4;
        }
    
      ops[0] = gen_rtx (MEM, HImode, 
			plus_constant (ld_addr_reg, ld_len - ld_offset));
      ops[1] = operands[ld_reg + 4];			 
      output_asm_insn ("ldh\t%1, %0", ops);
      
      ld_reg = (ld_reg + 1) % 3;
      ld_offset -= 2;
      delay_count++;
    }
  
  if (ld_offset >= 1)
    {
      /* if the load use delay has been met, I can start
         storing */
      if (delay_count >= 3)
        {
	  ops[0] = gen_rtx (MEM, SImode, 
			    plus_constant (st_addr_reg, st_len - st_offset));
	  ops[1] = operands[st_reg + 4];			 
	  output_asm_insn ("stw\t%1, %0", ops);
	  
	  st_reg = (st_reg + 1) % 3;
	  st_offset -= 4;
        }
    
      ops[0] = gen_rtx (MEM, QImode, 
			plus_constant (ld_addr_reg, ld_len - ld_offset));
      ops[1] = operands[ld_reg + 4];			 
      output_asm_insn ("ldb\t%1, %0", ops);
      
      ld_reg = (ld_reg + 1) % 3;
      ld_offset -= 1;
      delay_count++;
    }

    while (st_offset >= 4)
      {
	ops[0] = gen_rtx (MEM, SImode, 
			  plus_constant (st_addr_reg, st_len - st_offset));
	ops[1] = operands[st_reg + 4];			 
	output_asm_insn ("stw\t%1, %0", ops);

	st_reg = (st_reg + 1) % 3;
	st_offset -= 4;
      }
  
    while (st_offset >= 2)
      {
	ops[0] = gen_rtx (MEM, HImode, 
			  plus_constant (st_addr_reg, st_len - st_offset));
	ops[1] = operands[st_reg + 4];			 
	output_asm_insn ("sth\t%1, %0", ops);

	st_reg = (st_reg + 1) % 3;
	st_offset -= 2;
      }
  
    while (st_offset >= 1)
      {
	ops[0] = gen_rtx (MEM, QImode, 
			  plus_constant (st_addr_reg, st_len - st_offset));
	ops[1] = operands[st_reg + 4];			 
	output_asm_insn ("stb\t%1, %0", ops);

	st_reg = (st_reg + 1) % 3;
	st_offset -= 1;
      }
  
  return "";
}
; ??? lengths are not being used yet, but I will probably forget
; to update this once I am using lengths, so set it to something
; definetely big enough to cover it. 400 allows for 200 bytes
; of motion.
  [(set_attr "length" "400")])



;*****************************************************************************
;*
;* Custom instructions
;*
;*****************************************************************************

(define_constants [
  (CUSTOM_N 100)
  (CUSTOM_NI 101)
  (CUSTOM_NF 102)
  (CUSTOM_NP 103)
  (CUSTOM_NII 104)
  (CUSTOM_NIF 105)
  (CUSTOM_NIP 106)
  (CUSTOM_NFI 107)
  (CUSTOM_NFF 108)
  (CUSTOM_NFP 109)
  (CUSTOM_NPI 110)
  (CUSTOM_NPF 111)
  (CUSTOM_NPP 112)
  (CUSTOM_IN 113)
  (CUSTOM_INI 114)
  (CUSTOM_INF 115)
  (CUSTOM_INP 116)
  (CUSTOM_INII 117)
  (CUSTOM_INIF 118)
  (CUSTOM_INIP 119)
  (CUSTOM_INFI 120)
  (CUSTOM_INFF 121)
  (CUSTOM_INFP 122)
  (CUSTOM_INPI 123)
  (CUSTOM_INPF 124)
  (CUSTOM_INPP 125)
  (CUSTOM_FN 126)
  (CUSTOM_FNI 127)
  (CUSTOM_FNF 128)
  (CUSTOM_FNP 129)
  (CUSTOM_FNII 130)
  (CUSTOM_FNIF 131)
  (CUSTOM_FNIP 132)
  (CUSTOM_FNFI 133)
  (CUSTOM_FNFF 134)
  (CUSTOM_FNFP 135)
  (CUSTOM_FNPI 136)
  (CUSTOM_FNPF 137)
  (CUSTOM_FNPP 138)
  (CUSTOM_PN 139)
  (CUSTOM_PNI 140)
  (CUSTOM_PNF 141)
  (CUSTOM_PNP 142)
  (CUSTOM_PNII 143)
  (CUSTOM_PNIF 144)
  (CUSTOM_PNIP 145)
  (CUSTOM_PNFI 146)
  (CUSTOM_PNFF 147)
  (CUSTOM_PNFP 148)
  (CUSTOM_PNPI 149)
  (CUSTOM_PNPF 150)
  (CUSTOM_PNPP 151)
])


(define_insn "custom_n"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")] CUSTOM_N)]
  ""
  "custom\\t%0, zero, zero, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_ni"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")] CUSTOM_NI)]
  ""
  "custom\\t%0, zero, %1, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_nf"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SF 1 "register_operand"   "r")] CUSTOM_NF)]
  ""
  "custom\\t%0, zero, %1, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_np"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")] CUSTOM_NP)]
  ""
  "custom\\t%0, zero, %1, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_nii"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")
                     (match_operand:SI 2 "register_operand"   "r")] CUSTOM_NII)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_nif"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")
                     (match_operand:SF 2 "register_operand"   "r")] CUSTOM_NIF)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_nip"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")
                     (match_operand:SI 2 "register_operand"   "r")] CUSTOM_NIP)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_nfi"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SF 1 "register_operand"   "r")
                     (match_operand:SI 2 "register_operand"   "r")] CUSTOM_NFI)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_nff"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SF 1 "register_operand"   "r")
                     (match_operand:SF 2 "register_operand"   "r")] CUSTOM_NFF)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_nfp"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SF 1 "register_operand"   "r")
                     (match_operand:SI 2 "register_operand"   "r")] CUSTOM_NFP)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_npi"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")
                     (match_operand:SI 2 "register_operand"   "r")] CUSTOM_NPI)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_npf"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")
                     (match_operand:SF 2 "register_operand"   "r")] CUSTOM_NPF)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])

(define_insn "custom_npp"
  [(unspec_volatile [(match_operand:SI 0 "custom_insn_opcode" "N")
                     (match_operand:SI 1 "register_operand"   "r")
                     (match_operand:SI 2 "register_operand"   "r")] CUSTOM_NPP)]
  ""
  "custom\\t%0, zero, %1, %2"
  [(set_attr "type" "custom")])



(define_insn "custom_in"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")] CUSTOM_IN))]
  ""
  "custom\\t%1, %0, zero, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_ini"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")] CUSTOM_INI))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_inf"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")] CUSTOM_INF))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_inp"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")] CUSTOM_INP))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_inii"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_INII))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_inif"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_INIF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_inip"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_INIP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_infi"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_INFI))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_inff"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_INFF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_infp"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_INFP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_inpi"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_INPI))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_inpf"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_INPF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_inpp"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_INPP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])





(define_insn "custom_fn"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")] CUSTOM_FN))]
  ""
  "custom\\t%1, %0, zero, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_fni"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")] CUSTOM_FNI))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_fnf"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")] CUSTOM_FNF))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_fnp"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")] CUSTOM_FNP))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_fnii"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_FNII))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnif"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_FNIF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnip"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_FNIP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnfi"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_FNFI))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnff"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_FNFF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnfp"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_FNFP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnpi"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_FNPI))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnpf"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_FNPF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_fnpp"
  [(set (match_operand:SF 0 "register_operand"   "=r")
        (unspec_volatile:SF [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_FNPP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])



(define_insn "custom_pn"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")] CUSTOM_PN))]
  ""
  "custom\\t%1, %0, zero, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_pni"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")] CUSTOM_PNI))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_pnf"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")] CUSTOM_PNF))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_pnp"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")] CUSTOM_PNP))]
  ""
  "custom\\t%1, %0, %2, zero"
  [(set_attr "type" "custom")])

(define_insn "custom_pnii"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_PNII))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnif"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_PNIF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnip"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_PNIP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnfi"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_PNFI))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnff"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_PNFF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnfp"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SF 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_PNFP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnpi"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_PNPI))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnpf"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SF 3 "register_operand"   "r")] CUSTOM_PNPF))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])

(define_insn "custom_pnpp"
  [(set (match_operand:SI 0 "register_operand"   "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "custom_insn_opcode" "N")
                          (match_operand:SI 2 "register_operand"   "r")
                          (match_operand:SI 3 "register_operand"   "r")] CUSTOM_PNPP))]
  ""
  "custom\\t%1, %0, %2, %3"
  [(set_attr "type" "custom")])






;*****************************************************************************
;*
;* Misc
;*
;*****************************************************************************

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop\\t"
  [(set_attr "type" "alu")])

(define_insn "sync"
  [(unspec_volatile [(const_int 0)] UNSPEC_SYNC)]
  ""
  "sync\\t"
  [(set_attr "type" "control")])


(define_insn "rdctl"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "rdwrctl_operand" "O")] UNSPEC_RDCTL))]
  ""
  "rdctl\\t%0, ctl%1"
  [(set_attr "type" "control")])

(define_insn "wrctl"
  [(unspec_volatile:SI [(match_operand:SI 0 "rdwrctl_operand"  "O")
                        (match_operand:SI 1 "register_operand" "r")] UNSPEC_WRCTL)]
  ""
  "wrctl\\tctl%0, %1"
  [(set_attr "type" "control")])



;*****************************************************************************
;*
;* Peepholes
;*
;*****************************************************************************


