#!/bin/ksh

versions="current++ new++" 
src=`cmf_timeseries.json`
img_ext="png"
where='web'
interpretor='json'

dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .$ext`

for v in $versions
do
	echo $v
    if [ $v = "current++" ];
    then
       use magics++ 
	    version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
        ../upload.py $version ./$sf $s.$img_ext magics/reference/$version/$where -i $interpretor
    else
       use newmagics++ 
	    version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
        ../upload.py $version ./$sf $s.$img_ext magics/reference/$version/$where -i $interpretor
    fi

done
done
