#!/bin/ksh

set -A names  symbol
for name in ${names[*]}; do
  echo "processing $name"
  python $name.py
done

