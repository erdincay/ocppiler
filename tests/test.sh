#!/bin/bash

failed=false;

# TODO: fix this
./cppiler example1.arith > tests/tmp.txt
res="$(cat tests/tmp.txt)"
if ["$res" -ne 10 ]; then
  failed=true
  echo "Expected 10, instead got $res"
fi
