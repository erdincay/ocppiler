#!/bin/bash

failed=false;

# test1
./cppiler tests/test1.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test1.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test1.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test1.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test2
./cppiler tests/test2.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test2.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test2.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test2.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test3
./cppiler tests/test3.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test3.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test3.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test3.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test4
./cppiler tests/test4.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test4.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test4.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test4.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test5
./cppiler tests/test5.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test5.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test5.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test5.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test6
./cppiler tests/test6.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test6.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test6.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test6.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

#test7
./cppiler tests/test7.src -lex > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test7.lex.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler tests/test7.src -parse > tests/tmp.txt
res="$(diff -w tests/tmp.txt tests/test7.parse.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

if [ $failed = false ]; then
  echo 'All tests successful'
fi
