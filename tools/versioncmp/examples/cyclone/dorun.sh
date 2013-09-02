#!/bin/ksh
use pgf90
pgf90 -module $NAGINT -r8 -o cyclone1 ./cospe.f90 ./cyclone.f90  $MAGPLUSLIB_SHARED_DOUBLE $ECLIB $NAGLIB
./cyclone1 
mv ps.ps cyclone1.ps
pgf90 -module $NAGINT -r8 -o cyclone2 ./cospe2.f90 ./cyclone.f90  $MAGPLUSLIB_SHARED_DOUBLE $ECLIB $NAGLIB
./cyclone2 
mv ps.ps cyclone2.ps

