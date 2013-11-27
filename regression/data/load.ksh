#!/bin/ksh

versions="current++" 
versions="current++ new++" 
src=`ls *F`
output="ps" 
suffix="F" 
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
  s=`basename $sf .F`
for v in $versions
do
    grib=`grep grib $s.$suffix`
    grb=`grep grb $s.$suffix`
    nc=`grep nc $s.$suffix`
    x="add_magics_test(NAME $s SOURCES $s.$suffix IMAGE $s.$output DATA $grib $grb $nc THRESHOLD 50)"
    echo  "$x"
	/usr/local/apps/Magics/$v/bin/magics-config --compile=$s.$suffix --suffix=$suffix
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version ./$s $s.$output magics/reference/$version/data
done
done
