#!/bin/bash

# Simple sanity check.

HERE=`dirname \`readlink -f $0\``
cd $HERE

function test-ll () {
    touch test-system-1.lisp
    rm *.out *.err all-tests.output
    
    $LOADLISP --disable-debugger -T . -R load-lisp.test-system-1 --quit > test-1.out 2> test-1.err
    $LOADLISP --disable-debugger -T . -R load-lisp.test-system-1 ---main-1 ---main-2 --quit > test-1.out 2>> test-1.err
    cat test-1.out >> all-tests.output

    $LOADLISP --disable-debugger -T . -R load-lisp.test-system-1 --save-exe "a test.core" ---main-1 ---main-2 --quit > test-2.out 2>> test-2.err
    "./a test.core" > test-2.out 2>> test-2.err
    cat test-2.out >> all-tests.output

    $LOADLISP --core "a test.core" --disable-debugger ---main-1 ---main-2 --quit  > test-3.out 2>> test-3.er
    cat test-3.out >> all-tests.output
    diff -U 10 all-tests.output expected.output
}

LOADLISP=../load-lisp
test-ll

LOADLISP=../compile-lisp
test-ll
