#!/bin/ksh

versions="current++ new++" 
src="test01 test02 test03 test05 test06 test08" 
output="ps" 
suffix="f" 
dir=`pwd`
name=`basename $dir`


for s in $src 
do
for v in $versions
do
	echo $v
	/usr/local/apps/Magics/$v/bin/magics-config --compile=$s.$suffix --suffix=$suffix
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version $s $s.$output magics/reference/$version/venice
done
done
