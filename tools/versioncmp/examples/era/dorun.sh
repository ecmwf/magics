#!/bin/ksh

set -A names  era
for name in ${names[*]}; do
  echo "processing $name"
  python $name.py
done

