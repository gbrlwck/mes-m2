#/bin/env sh

RV64=../stage0-posix/x86
M2LIBC=../stage0-posix/M2libc/riscv64
# we need M1 binary in PATH and also guile
PATH=$RV64/bin:$PATH
MESC_DEBUG=1

guile \
    -L ../nyacc/module/                  \
    -L ./module/                         \
    -e main                              \
    scripts/mescc.scm                    \
    -D HAVE_CONFIG_H=1                   \
    -L lib/                              \
    -I include/                          \
    -I include/linux/riscv64/            \
    --arch=riscv64 --verbose             \
    --assemble $@
