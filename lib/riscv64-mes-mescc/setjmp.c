/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
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

#include <setjmp.h>
#include <stdlib.h>

void
longjmp (jmp_buf env, int val)
{
  val = val == 0 ? 1 : val;
  asm ("RD_T5 RS1_SP ADDI"); // save current stack pointer

  asm ("RD_SP !-192 ADDI"); // grow stack --> 24*8=192
  asm ("RS1_SP RS2_RA SD"); // save ra on stack
  asm ("RS1_SP RS2_FP @8 SD"); // save fp
  asm ("RS1_SP RS2_S1 @16 SD"); // save s1
  asm ("RS1_SP RS2_S2 @24 SD"); // save s2
  asm ("RS1_SP RS2_S3 @32 SD"); // save s3
  asm ("RS1_SP RS2_S4 @40 SD"); // save s4
  asm ("RS1_SP RS2_S5 @48 SD"); // save s5
  asm ("RS1_SP RS2_S6 @56 SD"); // save s6
  asm ("RS1_SP RS2_S7 @64 SD"); // save s7
  asm ("RS1_SP RS2_S8 @72 SD"); // save s8
  asm ("RS1_SP RS2_S9 @80 SD"); // save s9
  asm ("RS1_SP RS2_S10 @88 SD"); // save s10
  asm ("RS1_SP RS2_S11 @96 SD"); // save s11

  asm ("RS1_SP RS2_S0 @104 FSD"); // save fs0
  asm ("RS1_SP RS2_S1 @112 FSD"); // save fs1
  asm ("RS1_SP RS2_S2 @120 FSD"); // save fs2
  asm ("RS1_SP RS2_S3 @128 FSD"); // save fs3
  asm ("RS1_SP RS2_S4 @136 FSD"); // save fs4
  asm ("RS1_SP RS2_S5 @144 FSD"); // save fs5
  asm ("RS1_SP RS2_S6 @152 FSD"); // save fs6
  asm ("RS1_SP RS2_S7 @160 FSD"); // save fs7
  asm ("RS1_SP RS2_S8 @168 FSD"); // save fs8
  asm ("RS1_SP RS2_S9 @176 FSD"); // save fs9
  asm ("RS1_SP RS2_S10 @184 FSD"); // save fs10
  asm ("RS1_SP RS2_S11 @192 FSD"); // save fs11

  asm ("RD_FP RS1_SP !192 ADDI"); // set new fp

  asm ("RD_T5 RS1_T5 !8 LD"); // load target address from struct
  asm ("RD_RA RS1_T5 JALR");

  // not reached
  exit (42);
}

int
setjmp (__jmp_buf * env)
{
  long *p = (long *) &env;
  env[0].__pc = p[-2];
  env[0].__regs = p[-1];
  env[0].__sp = (long) &env;
  return 0;
}
