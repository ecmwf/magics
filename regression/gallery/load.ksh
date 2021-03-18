#!/bin/ksh

versions="current++" 
src=`ls *.py`

dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .py`

for v in $versions
do
	echo $v
    if [ $v = "current++" ];
    then
       use magics++ 
    else
       use newmagics++ 
    fi

	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
     ../upload.py $version ./$sf $s.png magics/reference/$version/efas -i python
done
done
