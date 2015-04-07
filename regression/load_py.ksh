#!/bin/ksh


set +x

versions="2.24.3" 
src="cmf_timeseries"
echo $src
git="/home/graphics/cgs/git/magics"

ext="json"
img_ext="png"
where='web'
interpretor='magjson'

dir=`pwd`
name=`basename $dir`



for sf in $src 
do
    s=`basename $sf .$ext`

for v in $versions
do
  module load Magics/$v
  echo "magics/reference/$v/$where"
  $git/regression/upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor

done
done
