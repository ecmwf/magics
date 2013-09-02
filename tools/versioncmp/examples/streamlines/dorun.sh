#!/bin/ksh

set -A names  streamlines
for name in ${names[*]}; do
  echo "processing $name"
  python $name.py
done

