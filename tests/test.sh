#!/bin/bash

failed=false;

./cppiler foo bar baz > tmp.txt
res="$(diff tmp.txt tests/test1.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler -length foo bar baz > tmp.txt
diff tmp.txt tests/test2.out
res="$(diff tmp.txt tests/test2.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler -help foo bar baz > tmp.txt
diff tmp.txt tests/test3.out
res="$(diff tmp.txt tests/test3.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler -length -help foo bar baz > tmp.txt
diff tmp.txt tests/test3.out
res="$(diff tmp.txt tests/test3.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

./cppiler -help -length foo bar baz > tmp.txt
diff tmp.txt tests/test3.out
res="$(diff tmp.txt tests/test3.out)"
if [ -n "$res" ]; then
  failed=true
  echo "$res"
fi

if [ $failed = false ]; then
  echo 'All tests successful'
fi
