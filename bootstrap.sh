# adapted from live-bootstrap/sysa/mes/mes.kaem

#alias mescc="${MES} -e main ${bindir}/mescc.scm -D HAVE_CONFIG_H=1 -I include -I include/linux/x86 -c"
alias mescc="./mescc-compile.sh"

# Start with crt1.o
mescc lib/linux/x86-mes-mescc/crt1.c

# Now for libc-mini.a
mescc lib/mes/eputs.c
mescc lib/mes/oputs.c
mescc lib/mes/globals.c
mescc lib/stdlib/exit.c
mescc lib/linux/x86-mes-mescc/_exit.c
mescc lib/linux/x86-mes-mescc/_write.c
mescc lib/stdlib/puts.c
mescc lib/string/strlen.c
mescc lib/mes/mini-write.c
catm ${libdir}/x86-mes/libc-mini.a eputs.o oputs.o globals.o exit.o _exit.o _write.o puts.o strlen.o mini-write.o
catm ${libdir}/x86-mes/libc-mini.s eputs.s oputs.s globals.s exit.s _exit.s _write.s puts.s strlen.s mini-write.s

# libmescc.a
mescc lib/linux/x86-mes-mescc/syscall-internal.c
catm ${libdir}/x86-mes/libmescc.a globals.o syscall-internal.o
catm ${libdir}/x86-mes/libmescc.s globals.s syscall-internal.s

# libc.a
mescc lib/ctype/isnumber.c
mescc lib/mes/abtol.c
mescc lib/mes/cast.c
mescc lib/mes/eputc.c
mescc lib/mes/fdgetc.c
mescc lib/mes/fdputc.c
mescc lib/mes/fdputs.c
mescc lib/mes/fdungetc.c
mescc lib/mes/itoa.c
mescc lib/mes/ltoa.c
mescc lib/mes/ltoab.c
mescc lib/mes/mes_open.c
mescc lib/mes/ntoab.c
mescc lib/mes/oputc.c
mescc lib/mes/ultoa.c
mescc lib/mes/utoa.c
mescc lib/ctype/isdigit.c
mescc lib/ctype/isspace.c
mescc lib/ctype/isxdigit.c
mescc lib/mes/assert_msg.c
mescc lib/posix/write.c
mescc lib/stdlib/atoi.c
mescc lib/linux/lseek.c
mescc lib/mes/__assert_fail.c
mescc lib/mes/__buffered_read.c
mescc lib/mes/__mes_debug.c
mescc lib/posix/execv.c
mescc lib/posix/getcwd.c
mescc lib/posix/getenv.c
mescc lib/posix/isatty.c
mescc lib/posix/open.c
mescc lib/posix/buffered-read.c
mescc lib/posix/setenv.c
mescc lib/posix/wait.c
mescc lib/stdio/fgetc.c
mescc lib/stdio/fputc.c
mescc lib/stdio/fputs.c
mescc lib/stdio/getc.c
mescc lib/stdio/getchar.c
mescc lib/stdio/putc.c
mescc lib/stdio/putchar.c
mescc lib/stdio/ungetc.c
mescc lib/stdlib/free.c
mescc lib/stdlib/malloc.c
mescc lib/stdlib/realloc.c
mescc lib/string/memchr.c
mescc lib/string/memcmp.c
mescc lib/string/memcpy.c
mescc lib/string/memmove.c
mescc lib/string/memset.c
mescc lib/string/strcmp.c
mescc lib/string/strcpy.c
mescc lib/string/strncmp.c
mescc lib/posix/raise.c
mescc lib/linux/access.c
mescc lib/linux/brk.c
mescc lib/linux/chmod.c
mescc lib/linux/clock_gettime.c
mescc lib/linux/dup.c
mescc lib/linux/dup2.c
mescc lib/linux/execve.c
mescc lib/linux/fork.c
mescc lib/linux/fsync.c
mescc lib/linux/_getcwd.c
mescc lib/linux/gettimeofday.c
mescc lib/linux/ioctl3.c
mescc lib/linux/_open3.c
mescc lib/linux/_read.c
mescc lib/linux/time.c
mescc lib/linux/unlink.c
mescc lib/linux/waitpid.c
mescc lib/linux/x86-mes-mescc/syscall.c
mescc lib/linux/getpid.c
mescc lib/linux/kill.c
catm ${libdir}/x86-mes/libc.a eputs.o oputs.o globals.o exit.o _exit.o _write.o puts.o strlen.o isnumber.o abtol.o cast.o eputc.o fdgetc.o fdputc.o fdputs.o fdungetc.o itoa.o ltoa.o ltoab.o mes_open.o ntoab.o oputc.o ultoa.o utoa.o isdigit.o isspace.o isxdigit.o assert_msg.o write.o atoi.o lseek.o __assert_fail.o __buffered_read.o __mes_debug.o execv.o getcwd.o getenv.o isatty.o open.o buffered-read.o setenv.o wait.o fgetc.o fputc.o fputs.o getc.o getchar.o putc.o putchar.o ungetc.o free.o malloc.o realloc.o memchr.o memcmp.o memcpy.o memmove.o memset.o strcmp.o strcpy.o strncmp.o raise.o access.o brk.o chmod.o clock_gettime.o dup.o dup2.o execve.o fork.o fsync.o _getcwd.o gettimeofday.o ioctl3.o _open3.o _read.o time.o unlink.o waitpid.o syscall.o getpid.o kill.o
catm ${libdir}/x86-mes/libc.s eputs.s oputs.s globals.s exit.s _exit.s _write.s puts.s strlen.s isnumber.s abtol.s cast.s eputc.s fdgetc.s fdputc.s fdputs.s fdungetc.s itoa.s ltoa.s ltoab.s mes_open.s ntoab.s oputc.s ultoa.s utoa.s isdigit.s isspace.s isxdigit.s assert_msg.s write.s atoi.s lseek.s __assert_fail.s __buffered_read.s __mes_debug.s execv.s getcwd.s getenv.s isatty.s open.s buffered-read.s setenv.s wait.s fgetc.s fputc.s fputs.s getc.s getchar.s putc.s putchar.s ungetc.s free.s malloc.s realloc.s memchr.s memcmp.s memcpy.s memmove.s memset.s strcmp.s strcpy.s strncmp.s raise.s access.s brk.s chmod.s clock_gettime.s dup.s dup2.s execve.s fork.s fsync.s _getcwd.s gettimeofday.s ioctl3.s _open3.s _read.s time.s unlink.s waitpid.s syscall.s getpid.s kill.s

# libc+tcc.a
mescc lib/ctype/islower.c
mescc lib/ctype/isupper.c
mescc lib/ctype/tolower.c
mescc lib/ctype/toupper.c
mescc lib/mes/abtod.c
mescc lib/mes/dtoab.c
mescc lib/mes/search-path.c
mescc lib/posix/execvp.c
mescc lib/stdio/fclose.c
mescc lib/stdio/fdopen.c
mescc lib/stdio/ferror.c
mescc lib/stdio/fflush.c
mescc lib/stdio/fopen.c
mescc lib/stdio/fprintf.c
mescc lib/stdio/fread.c
mescc lib/stdio/fseek.c
mescc lib/stdio/ftell.c
mescc lib/stdio/fwrite.c
mescc lib/stdio/printf.c
mescc lib/stdio/remove.c
mescc lib/stdio/snprintf.c
mescc lib/stdio/sprintf.c
mescc lib/stdio/sscanf.c
mescc lib/stdio/vfprintf.c
mescc lib/stdio/vprintf.c
mescc lib/stdio/vsnprintf.c
mescc lib/stdio/vsprintf.c
mescc lib/stdio/vsscanf.c
mescc lib/stdlib/calloc.c
mescc lib/stdlib/qsort.c
mescc lib/stdlib/strtod.c
mescc lib/stdlib/strtof.c
mescc lib/stdlib/strtol.c
mescc lib/stdlib/strtold.c
mescc lib/stdlib/strtoll.c
mescc lib/stdlib/strtoul.c
mescc lib/stdlib/strtoull.c
mescc lib/string/memmem.c
mescc lib/string/strcat.c
mescc lib/string/strchr.c
mescc lib/string/strlwr.c
mescc lib/string/strncpy.c
mescc lib/string/strrchr.c
mescc lib/string/strstr.c
mescc lib/string/strupr.c
mescc lib/stub/sigaction.c
mescc lib/stub/ldexp.c
mescc lib/stub/mprotect.c
mescc lib/stub/localtime.c
mescc lib/stub/sigemptyset.c
mescc lib/x86-mes-mescc/setjmp.c
mescc lib/linux/close.c
mescc lib/linux/rmdir.c
mescc lib/linux/stat.c
catm ${libdir}/x86-mes/libc+tcc.a ${libdir}/x86-mes/libc.a islower.o isupper.o tolower.o toupper.o abtod.o dtoab.o search-path.o execvp.o fclose.o fdopen.o ferror.o fflush.o fopen.o fprintf.o fread.o fseek.o ftell.o fwrite.o printf.o remove.o snprintf.o sprintf.o sscanf.o vfprintf.o vprintf.o vsnprintf.o vsprintf.o vsscanf.o calloc.o qsort.o strtod.o strtof.o strtol.o strtold.o strtoll.o strtoul.o strtoull.o memmem.o strcat.o strchr.o strlwr.o strncpy.o strrchr.o strstr.o strupr.o sigaction.o ldexp.o mprotect.o localtime.o sigemptyset.o setjmp.o close.o rmdir.o stat.o
catm ${libdir}/x86-mes/libc+tcc.s ${libdir}/x86-mes/libc.s islower.s isupper.s tolower.s toupper.s abtod.s dtoab.s search-path.s execvp.s fclose.s fdopen.s ferror.s fflush.s fopen.s fprintf.s fread.s fseek.s ftell.s fwrite.s printf.s remove.s snprintf.s sprintf.s sscanf.s vfprintf.s vprintf.s vsnprintf.s vsprintf.s vsscanf.s calloc.s qsort.s strtod.s strtof.s strtol.s strtold.s strtoll.s strtoul.s strtoull.s memmem.s strcat.s strchr.s strlwr.s strncpy.s strrchr.s strstr.s strupr.s sigaction.s ldexp.s mprotect.s localtime.s sigemptyset.s setjmp.s close.s rmdir.s stat.s

# Make directories
mkdir ${prefix}/lib/linux ${incdir}/mes ${incdir}/sys ${incdir}/linux
mkdir ${prefix}/lib/x86-mes ${prefix}/lib/linux/x86-mes ${incdir}/linux/x86

# Install libraries
cp ${libdir}/x86-mes/libc.a ${prefix}/lib/x86-mes/
cp ${libdir}/x86-mes/libc+tcc.a ${prefix}/lib/x86-mes/
cp ${libdir}/x86-mes/libmescc.a ${prefix}/lib/x86-mes/
cp ${libdir}/x86-mes/libc.s ${prefix}/lib/x86-mes/
cp ${libdir}/x86-mes/libc+tcc.s ${prefix}/lib/x86-mes/
cp ${libdir}/x86-mes/libmescc.s ${prefix}/lib/x86-mes/
cp ${libdir}/x86-mes/x86.M1 ${prefix}/lib/x86-mes/
cp crt1.o ${prefix}/lib/x86-mes/
cp crt1.s ${prefix}/lib/x86-mes/
cp ${libdir}/linux/x86-mes/elf32-footer-single-main.hex2 ${prefix}/lib/linux/x86-mes/
cp ${libdir}/linux/x86-mes/elf32-header.hex2 ${prefix}/lib/linux/x86-mes/

# Install header files
cp include/alloca.h ${incdir}/alloca.h
cp include/argz.h ${incdir}/argz.h
cp include/ar.h ${incdir}/ar.h
cp include/assert.h ${incdir}/assert.h
cp include/ctype.h ${incdir}/ctype.h
cp include/dirent.h ${incdir}/dirent.h
cp include/dirstream.h ${incdir}/dirstream.h
cp include/dlfcn.h ${incdir}/dlfcn.h
cp include/endian.h ${incdir}/endian.h
cp include/errno.h ${incdir}/errno.h
cp include/fcntl.h ${incdir}/fcntl.h
cp include/features.h ${incdir}/features.h
cp include/float.h ${incdir}/float.h
cp include/getopt.h ${incdir}/getopt.h
cp include/grp.h ${incdir}/grp.h
cp include/inttypes.h ${incdir}/inttypes.h
cp include/libgen.h ${incdir}/libgen.h
cp include/limits.h ${incdir}/limits.h
cp include/locale.h ${incdir}/locale.h
cp include/math.h ${incdir}/math.h
cp include/memory.h ${incdir}/memory.h
cp include/pwd.h ${incdir}/pwd.h
cp include/setjmp.h ${incdir}/setjmp.h
cp include/signal.h ${incdir}/signal.h
cp include/stdarg.h ${incdir}/stdarg.h
cp include/stdbool.h ${incdir}/stdbool.h
cp include/stddef.h ${incdir}/stddef.h
cp include/stdint.h ${incdir}/stdint.h
cp include/stdio.h ${incdir}/stdio.h
cp include/stdlib.h ${incdir}/stdlib.h
cp include/stdnoreturn.h ${incdir}/stdnoreturn.h
cp include/string.h ${incdir}/string.h
cp include/strings.h ${incdir}/strings.h
cp include/termio.h ${incdir}/termio.h
cp include/time.h ${incdir}/time.h
cp include/unistd.h ${incdir}/unistd.h

cp include/linux/syscall.h ${incdir}/linux/syscall.h
cp include/linux/x86/syscall.h ${incdir}/linux/x86/syscall.h

cp include/mes/builtins.h ${incdir}/mes/builtins.h
cp include/mes/cc.h ${incdir}/mes/cc.h
catm ${incdir}/mes/config.h
cp include/mes/constants.h ${incdir}/mes/constants.h
cp include/mes/lib.h ${incdir}/mes/lib.h
cp include/mes/lib-mini.h ${incdir}/mes/lib-mini.h
cp include/mes/mes.h ${incdir}/mes/mes.h
cp include/mes/symbols.h ${incdir}/mes/symbols.h

cp include/sys/cdefs.h ${incdir}/sys/cdefs.h
cp include/sys/dir.h ${incdir}/sys/dir.h
cp include/sys/file.h ${incdir}/sys/file.h
cp include/sys/ioctl.h ${incdir}/sys/ioctl.h
cp include/sys/mman.h ${incdir}/sys/mman.h
cp include/sys/param.h ${incdir}/sys/param.h
cp include/sys/resource.h ${incdir}/sys/resource.h
cp include/sys/select.h ${incdir}/sys/select.h
cp include/sys/stat.h ${incdir}/sys/stat.h
cp include/sys/timeb.h ${incdir}/sys/timeb.h
cp include/sys/time.h ${incdir}/sys/time.h
cp include/sys/times.h ${incdir}/sys/times.h
cp include/sys/types.h ${incdir}/sys/types.h
cp include/sys/ucontext.h ${incdir}/sys/ucontext.h
cp include/sys/user.h ${incdir}/sys/user.h
cp include/sys/wait.h ${incdir}/sys/wait.h

# Checksums
cd ${sources}/${MES_PKG}
sha256sum -c checksums
