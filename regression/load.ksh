#!/bin/ksh

set +x

src='wind-ecchart.json'
img_ext="png"
where='web'
interpretor='magjson'
ext="json"

version="2.24.7"
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .$ext`

    module load Magics/$version
    ../upload.py $version ./$sf $s.$img_ext magics/reference/$version/$where -i $interpretor

done
