#!/bin/ksh

set +x

src='big_mercator.json'
img="big_mercator.png"
where='proj4'
interpretor='magjsonx'

version="3.0.0"
dir=`pwd`
name=`basename $dir`
touch $img

for sf in $src 
do
    s=`basename $sf .$ext`

    module load Magics/$version
     /tmp/cgs/git/magics-develop/regression/upload.py $version ./$sf $img magics/reference/$version/$where -i $interpretor
done
