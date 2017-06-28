#!/bin/ksh

set +x

src='mollweide-frame.json'
img="mollweide-frame.png"
where='proj4'
interpretor='magjsonx'

version="2.33.0"
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
    s=`basename $sf .$ext`

    module load Magics/$version
    ../upload.py $version ./$sf $img magics/reference/$version/$where -i $interpretor
done
