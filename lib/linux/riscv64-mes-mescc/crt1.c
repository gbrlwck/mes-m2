/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
 * Copyright © 2021 Gabriel Wicki <gabriel@erlikon.ch>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "mes/lib-mini.h"
int main (int argc, char *argv[], char *envp[]);

/* mesc will generate the following preamble: ??? probably not
also: it's "mescc"

   push    ra
   push    fp
*/

int
_start ()
{
  asm ("RD_T0 $i16_000 @0 ADDI"); //asm ("li_____%t0,$i16_0000 @0");
  asm ("RD_T1 $i32 &__stdin ADDI");// asm ("li_____%t1,$i32 &__stdin");
  asm ("RD_T0 RS1_T1 SW"); //asm ("sw_____%t0,0(%t1)");

  asm ("RD_T0 $i16_000 @1 ADDI"); //asm ("li_____%t0,$i16_0000 @1");
  asm ("RD_T0 !16 SRAI"); //asm ("srai___%t0,16");
  asm ("RD_T1 $i32 &__stdout ADDI");// asm ("li_____%t1,$i32 &__stdout");
  asm ("RD_T0 RS1_T1 SW");// asm ("sw_____%t0,0(%t1)");

  asm ("RD_T0 $i16_000 @2 ADDI"); //asm ("li_____%t0,$i16_0000 @2");
  asm ("RD_T0 !16 SRAI"); //asm ("srai___%t0,16");
  asm ("RD_T1 $i32 &__stderr ADDI");// asm ("li_____%t1,$i32 &__stderr");
  asm ("RD_01 RS1_T1 SW"); // asm ("sw_____%t0,0(%t1)");

  // environ is &argv[argc + 1]
  asm ("RS1_FP RD_T1 ADDI"); //asm ("mv_____%t1,%fp");


  asm ("RD_T1 RS1_T1 $i8_8 !0x1 ADDI");// asm ("addi___%t1,%t1,$i8_8 !0x1"); // 0x10 to skip over pushed fp+ra, 0x8 to skip over argc
  asm ("RD_T5 RS1_FP $i8_0 !0x1 ADDI");// asm ("addi___%t5,%fp,$i8_0 !0x1"); // 0x10 to skip over pushed fp+ra
  asm ("RD_T0 RS1_T5 LD");// asm ("ld_____%t0,0(%t5)");
  asm ("TD_T0 RS1_T0 !1 ADDI");// asm ("addi___%t0,%t0,1");
  asm ("RD_T5 $i32 %0x3 ADDI");// asm ("li_____%t5,$i32 %0x3"); // skip over all arguments and the final NULL
  asm ("RD_T0 RS1_T0 RS2_T5 SLL"); //asm ("sll____%t0,%t0,%t5");
  asm ("RD_T0 RS1_T0 RS2_T1 ADD"); // asm ("add____%t0,%t0,%t1");

  //asm ("push___%t0"); // envp
  asm ("RD_SP RS1_SP -8 ADDI"); // alloc stack
  asm ("RD_T0 RS1_SP SD"); // save t0 on stack

  //asm ("push___%t1"); // argv
  asm ("RD_SP RS1_SP -8 ADDI"); // alloc stack
  asm ("RD_T1 RS1_SP SD"); // save t0 on stack

  asm ("RD_T1 $i32 &environ ADDI"); // asm ("li_____%t1,$i32 &environ");

  asm ("RS1_T1 RD_T0 SD"); //asm ("sd_____%t0,0(%t1)");

  asm ("RD_T5 RS1_FP $i8_0 !0x1 ADDI");// asm ("addi___%t5,%fp,$i8_0 !0x1"); // 0x10 to skip over pushed fp+ra

  asm ("RD_T0 RS1_T5 LD"); //asm ("ld_____%t0,0(%t5)");

  //asm ("push___%t0"); // argc
  asm ("RD_SP RS1_SP -8 ADDI"); // alloc stack
  asm ("RD_T0 RS1_SP SD"); // save t0 on stack

  main ();

  // asm ("mv_____%a0,%t0");
  // asm ("li_____%a7,SYS_exit");
  asm ("RS1_T0 RD_A0 ADDI");
  asm ("RD_A7 SYS_exit ADDI");
  asm ("ECALL");
  asm ("EBREAK");
}
