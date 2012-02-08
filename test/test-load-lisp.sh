#!/bin/bash

# Simple sanity check.

HERE=`dirname \`readlink -f $0\``
cd $HERE

function test-ll () {
    touch test-system-1.lisp
    rm test-1.output test-1.error test-2.output test-2.error all-tests.output
    
    $LOADLISP --disable-debugger -T . -R load-lisp.test-system-1 --quit > test-1.output 2> test-1.error
    $LOADLISP --disable-debugger -T . -R load-lisp.test-system-1 ---main-1 ---main-2 --quit > test-1.output 2>> test-1.error
    cat test-1.output >> all-tests.output

    $LOADLISP --disable-debugger -T . -R load-lisp.test-system-1 --save-exe test.core ---main-1 ---main-2 --quit > test-2.output 2>> test-2.error
    ./test.core > test-2.output 2>> test-2.error
    cat test-2.output >> all-tests.output

    diff -U 10 all-tests.output expected.output
}

LOADLISP=../load-lisp
test-ll

LOADLISP=../compile-lisp
test-ll
