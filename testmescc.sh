#/bin/env bash

TEST_DIR=lib/tests/scaffold/

for file in ${TEST_DIR}*.c 
do
    if ! $(./mescc-compile.sh $file); then
        echo $file "didn't compile"
        gcc -O0 -S $file
        more $(basename "${file%.c}").s
        exit -1
    fi
done

exit 0
