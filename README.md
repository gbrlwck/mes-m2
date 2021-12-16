mes-m2
======

Making GNU MES bootstrappable by adjusting mes.c

Goal
----

The goal of this project is to provide a minimally-adapted GNU MES which can be compiled by M2-Planet
and is able to compile the real GNU Mes (and thereby run MEScc).

Status
------

Work in progress.

Completed for x86 but porting to M2libc to accelerate architecture porting is not done.

Installation
------------

Just run:
$ make mes-m2-boot

If you want to bootstrap the build (using the M2/stage0 toolchain) just make sure you have the ARCH
and PATH environment variables set, for example like:
$ ARCH=x86 PATH=$PATH:../stage0-posix/x86/bin/ make mes-m2-boot


Running MES
-----------

To run mes successfully, the following environment variables have to be set (these should be adequate defaults):
 - MES_PREFIX
 - GUILE_LOAD_PATH=${MES_PREFIX}/mes/module:${MES_PREFIX}/module:../nyacc/module (this is equivalent to using the -L command-line option)
 - MES_ARENA=20000000
 - MES_MAX_ARENA=20000000
 - MES_STACK=6000000

Running MEScc
-------------

You can try running mescc like this

```ShellSession
$ MES_PREFIX=. MES_ARENA=20000000 MES_MAX_ARENA=20000000 MES_STACK=6000000 ./bin/mes --no-auto-compile -L module -L ../nyacc/module -e main scripts/mescc.scm -I include -v -S scaffold/exit-42.c -o foo.S
```

You can also compare the execution with Guile (spoiler: after mescc is compiled it's way faster).  Run

```ShellSession
$ guile --no-auto-compile -L module -L ../nyacc/module -e main scripts/mescc.scm -I include -v -S scaffold/exit-42.c -o foo.S
```

to compile using the MesCC and nyacc included here.


To actually *build* an executable, you'll have to omit the -S option (which stands for compile) and add lib/ to the library path (via -L /after/ scripts/mescc.scm)
```ShellSession
$ guile --no-auto-compile -L module -L ../nyacc/module -e main scripts/mescc.scm -I include -L lib -v scaffold/exit-42.c -o foo.S
```
