#!/bin/ksh

versions="current++ new++" 
src="rotated.py"
output="rotated.ps" 
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
for v in $versions
do
	echo $v
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version ./$sf $output magics/reference/$version/rotated -i python
done
done
