#!/bin/ksh

versions="current++ new++" 
src=`ls projection*.py`

dir=`pwd`
name=`basename $dir`

echo "" > CMakeLists.txt2

for sf in $src 
do
    s=`basename $sf .py`

for v in $versions
do
	echo $v
    if [ $v = "current++" ];
    then
       use magics++ 
       echo $x >> CMakeLists.txt2
    else
       use newmagics++ 
    fi

	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version ./$sf $s.png magics/reference/$version/gallery -i python
done
done
