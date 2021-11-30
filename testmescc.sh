#/bin/env bash

TEST_DIR=lib/tests/scaffold/
GCC=gcc
DO_GCC=0

PATH=../stage0-posix/x86/bin:$PATH

for file in ${TEST_DIR}*.c
do
    echo "compiling: " $file
    if ! $(./mescc-compile.sh $file); then
        echo $file "didn't compile"
        if [ ! $DO_GCC ]; then
            $GCC -O0 -S $file
            more $(basename "${file%.c}").s
        fi
        exit -1
    fi
done

exit 0
