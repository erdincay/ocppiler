#!/bin/bash

failed=false;

# test1
./compiler.native -parse tests/test1.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test1.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test2
./compiler.native -parse tests/test2.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test2.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test3
./compiler.native -parse tests/test3.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test3.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test4
./compiler.native -parse tests/test4.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test4.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test5
./compiler.native -parse tests/test5.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test5.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test6
./compiler.native -parse tests/test6.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test6.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test7
./compiler.native -parse tests/test7.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test7.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test8
./compiler.native -parse tests/test8.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test8.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test9
./compiler.native -parse tests/test9.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test9.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test10
./compiler.native -parse tests/test10.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test10.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test11
./compiler.native -parse tests/test11.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test11.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test12
./compiler.native -parse tests/test12.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test12.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test13
./compiler.native -parse tests/test13.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test13.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test14
./compiler.native -parse tests/test14.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test14.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test15
./compiler.native -parse tests/test15.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test15.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test16
./compiler.native -parse tests/test16.src > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test16.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

if [ $failed = false ]; then
  echo 'All tests successful'
fi
