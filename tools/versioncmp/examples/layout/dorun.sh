#!/bin/ksh
use pgf90
pgf90 -module $NAGINT -r8 -o exe ./layout.f  $MAGPLUSLIB_SHARED_DOUBLE $ECLIB $NAGLIB
./exe DEVICE PNG
