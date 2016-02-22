#!/bin/ksh

set +x

src='wave-dir-land.json'
img_ext="png"
img="wave-dir-land.png"
where='web'
interpretor='magjson'
ext="json"

version="2.26.0"
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .$ext`

    module load Magics/$version
    ../upload.py $version ./$sf $img magics/reference/$version/$where -i $interpretor
done
