/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

void
_write (int filedes, void const *buffer, size_t size)
{
  asm ("RD_A0 RS1_FP 0x10 LD"); //asm ("ld_____%a0,0x10(%fp)");
  asm ("RD_A1 RS1_FP 0x18 LD"); //asm ("ld_____%a1,0x18(%fp)");
  asm ("RD_A2 RS1_FP 0x20 LD"); //asm ("ld_____%a2,0x20(%fp)");
  asm ("RD_A7 SYS_write ADDI"); //asm ("li_____%a7,SYS_write");
  asm ("ECALL"); //asm ("ecall");
  asm ("RD_T0 RS1_A0 ADDI"); //asm ("mv_____%t0,%a0");
}
