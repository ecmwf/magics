#!/bin/ksh

versions="current++ new++" 
src=`ls *.F`
output="ps" 
suffix="F" 
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
  s=`basename $sf .F`
  echo $s
for v in $versions
do
	echo $v
	/usr/local/apps/Magics/$v/bin/magics-config --compile=$s.$suffix --suffix=$suffix
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version ./$s $s.$output magics/reference/$version/coast
done
done
