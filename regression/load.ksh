#!/bin/ksh

versions="current++ new++" 
src=`ls 15days*.json`
where='web'

dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .json`

for v in $versions
do
	echo $v
    if [ $v = "current++" ];
    then
       use magics++ 
	    version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
    else
       use newmagics++ 
	   version='2.20.2'
    fi

     ../upload.py $version ./$sf $s.png magics/reference/$version/$where -i magjson
done
done
