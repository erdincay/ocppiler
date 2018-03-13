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

if [ $failed = false ]; then
  echo 'All tests successful'
fi
