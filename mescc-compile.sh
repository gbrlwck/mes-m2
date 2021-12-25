#/bin/env sh

RV64=/root/stage0-posix/riscv64
M2LIBC=/root/stage0-posix/M2libc/riscv64
# we need M1 binary in PATH and also guile
PATH=$RV64/bin:$PATH
MESC_DEBUG=1

guile \
    -L /root/nyacc/module/               \
    -L /root/mes-m2/module/              \
    -e main                              \
    scripts/mescc.scm                    \
    -D HAVE_CONFIG_H=1                   \
    -I include/                          \
    -I include/linux/riscv64/            \
    --arch=riscv64 --verbose             \
    -c $@
