/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __MES_STDIO_H
#define __MES_STDIO_H 1

char **environ;
int g_stdin;
int g_stdout;

#ifndef STDIN
#define STDIN 0
#endif

#ifndef STDOUT
#define STDOUT 1
#endif

#ifndef STDERR
#define STDERR 2
#endif

#if WITH_GLIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_STDIO_H
#include_next <stdio.h>

#else // ! WITH_GLIBC

#ifndef __MESCCLIB__
#define __MESCCLIB__ 15
#endif

#ifndef EOF
#define EOF -1
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef BUFSIZ
#define BUFSIZ 256
#endif

#ifndef L_tmpnam
#define L_tmpnam 100
#endif

#if !defined (__MES_FILE_T) && ! defined (_FILE_T)
#define __MES_FILE_T
#define _FILE_T
typedef int FILE;
#endif

#define stdin (FILE*)0
#define stdout (FILE*)1
#define stderr (FILE*)2

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif

FILE *fdopen (int fd, char const *mode);
FILE *fopen (char const *file_name, char const *mode);
int eputc (int c);
int eputs (char const* s);
int fclose (FILE *stream);
int feof (FILE *stream);
int ferror (FILE *stream);
int fflush (FILE *stream);
int fgetc (FILE* stream);
char *fgets (char *s, int size, FILE *stream);
int fprintf (FILE *stream, char const *format, ...);
int fpurge (FILE *stream);
int fputc (int c, FILE *stream);
int fputs (char const* s, FILE *stream);
int fseek (FILE *stream, long offset, int whence);
int getc (FILE *stream);
int getchar ();
int printf (char const* format, ...);
int putc (int c, FILE* stream);
int putchar (int c);
int puts (char const* s);
int remove (char const *file_name);
int snprintf(char *str,  size_t size,  char const *format, ...);
int sprintf (char *str, char const* format, ...);
int sscanf (char const *str, const char *format, ...);
int ungetc (int c, FILE* stream);
long ftell (FILE *stream);
size_t fread (void *ptr, size_t size, size_t count, FILE *stream);
size_t freadahead (FILE *fp);
size_t fwrite (void const *ptr, size_t size, size_t count, FILE *stream);

#endif // ! WITH_GLIBC

#endif // __MES_STDIO_H