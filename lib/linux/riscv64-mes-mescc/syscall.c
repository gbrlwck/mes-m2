/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <errno.h>
#include <linux/riscv64/syscall.h>

long
__sys_call (long sys_call)
{
  asm ("RD_A7 RS1_FP !-8 LD");
  asm ("ECALL");
  asm ("RD_T0 RS1_A0 ADDI");
}

long
__sys_call1 (long sys_call, long one)
{
  asm ("RD_A7 RS1_FP !-8 LD");
  asm ("RD_A0 RS1_FP !-16 LD");
  asm ("ECALL");
  asm ("RD_T0 RS1_A0 ADDI");
}

long
__sys_call2 (long sys_call, long one, long two)
{
  asm ("RD_A7 RS1_FP !-8 LD");
  asm ("RD_A0 RS1_FP !-16 LD");
  asm ("RD_A1 RS1_FP !-24 LD");
  asm ("ECALL");
  asm ("RD_T0 RS1_A0 ADDI");
}

long
__sys_call3 (long sys_call, long one, long two, long three)
{
  asm ("RD_A7 RS1_FP !-8 LD");
  asm ("RD_A0 RS1_FP !-16 LD");
  asm ("RD_A1 RS1_FP !-24 LD");
  asm ("RD_A2 RS1_FP !-32 LD");
  asm ("ECALL");
  asm ("RD_T0 RS1_A0 ADDI");
}

long
__sys_call4 (long sys_call, long one, long two, long three, long four)
{
  asm ("RD_A7 RS1_FP !-8 LD");
  asm ("RD_A0 RS1_FP !-16 LD");
  asm ("RD_A1 RS1_FP !-32 LD");
  asm ("RD_A2 RS1_FP !-40 LD");
  asm ("RD_A3 RS1_FP !-48 LD");
  asm ("ECALL");
  asm ("RD_T0 RS1_A0 ADDI");
}

long
__sys_call5 (long sys_call, long one, long two, long three, long four, long five)
{
  asm ("RD_A7 RS1_FP !-8 LD");
  asm ("RD_A0 RS1_FP !-16 LD");
  asm ("RD_A1 RS1_FP !-32 LD");
  asm ("RD_A2 RS1_FP !-40 LD");
  asm ("RD_A3 RS1_FP !-48 LD");
  asm ("RD_A4 RS1_FP !-56 LD");
  asm ("ECALL");
  asm ("RD_T0 RS1_A0 ADDI");
}

long
_sys_call (long sys_call)
{
  long r = __sys_call (sys_call);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call1 (long sys_call, long one)
{
  long r = __sys_call1 (sys_call, one);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call2 (long sys_call, long one, long two)
{
  long r = __sys_call2 (sys_call, one, two);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call3 (long sys_call, long one, long two, long three)
{
  long r = __sys_call3 (sys_call, one, two, three);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r = __sys_call4 (sys_call, one, two, three, four);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call5 (long sys_call, long one, long two, long three, long four, long five)
{
  long r = __sys_call5 (sys_call, one, two, three, four, five);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}
