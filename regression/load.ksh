#!/bin/ksh

set +x

src='cmf_advanced_timeseries.json'
img_ext="png"
img="cmf_advanced_timeseries.png"
where='web'
interpretor='python'
ext="json"

version="2.30.0"
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .$ext`

    module load Magics/$version
    ../upload.py $version ./$sf $img magics/reference/$version/$where -i $interpretor
done
