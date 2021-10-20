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


/* copied from mes/lib/linux/riscv64-mes-gcc/_exit.c */

#define SYS_exit 93

void
_exit ()
{
  asm("RD_A7 !93 ADDI" /* SYS_exit 93 */
      "ECALL");
  /* not reached */
  _exit (0);
}

/* copied from mes/lib/linux/riscv64-mes-gcc/_write.c */

/* not sure how to use pre-processor #define within an asm() call */
#define SYS_write 64

ssize_t
_write (int filedes, void const *buffer, size_t size)
{
  /* arguments are passed to functions in registers a0..aN
     so my guess is, they do not have to be moved before the syscall for they
     remain in the same place
  */
  asm("RD_A7 !64 ADDI"  
      "ECALL");
}
