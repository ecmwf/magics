#!/bin/ksh


set +x

versions="2.24.5" 
src="geojson_multilines.py"
echo $src
git="/home/graphics/cgs/git/magics"

ext="py"
img_ext="png"
where='gallery'
interpretor='python'

dir=`pwd`
name=`basename $dir`



for sf in $src 
do
    s=`basename $sf .$ext`

for v in $versions
do
  echo "magics/reference/$v/$where"
  $git/regression/upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor

done
done
