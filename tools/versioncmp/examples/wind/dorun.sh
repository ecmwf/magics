#!/bin/ksh
use pgf90
pgf90 -module $NAGINT -r8 -o exe ./wind.f  $MAGPLUSLIB_SHARED_DOUBLE $ECLIB $NAGLIB
./exe DEVICE PNG
