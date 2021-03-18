#!/bin/ksh

versions="current++ new++" 
src="axis01 axis02 automatic_axis" 
output="ps" 
suffix="F" 
dir=`pwd`
name=`basename $dir`


for s in $src 
do
for v in $versions
do
	echo $v
	/usr/local/apps/Magics/$v/bin/magics-config --compile=$s.$suffix --suffix=$suffix
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version $s $s.$output magics/reference/$version/axis
done
done
