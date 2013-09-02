#!/bin/ksh
use pgf90
pgf90 -module $NAGINT -r8 -o exe ./land.f  $MAGPLUSLIB_SHARED_DOUBLE $ECLIB $NAGLIB
./exe
