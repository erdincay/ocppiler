#!/bin/bash

failed=false;

# test1
./compiler.native tests/test1.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test1.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./compiler.native tests/test1.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test1.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test2
./compiler.native tests/test2.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test2.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./compiler.native tests/test2.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test2.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test3
./compiler.native tests/test3.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test3.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./compiler.native tests/test3.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test3.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test4
./compiler.native tests/test4.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test4.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./compiler.native tests/test4.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test4.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test5
./compiler.native tests/test5.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test5.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./compiler.native tests/test5.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test5.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test6
./compiler.native tests/test6.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test6.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./compiler.native tests/test6.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test6.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

if [ $failed = false ]; then
  echo 'All tests successful'
fi
