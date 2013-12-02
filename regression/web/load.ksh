#!/bin/ksh

versions="current++ new++" 
src="epsgram_sample.py"
output="epsgram_sample.ps" 
suffix="F" 
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
for v in $versions
do
	echo $v
	#/usr/local/apps/Magics/$v/bin/magjson $sf 
    python $sf 
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version ./$sf $output magics/reference/$version/web -i python
done
done
