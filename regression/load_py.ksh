#!/bin/ksh


set +x

versions="2.24.4" 
src="wind_thinning.py wind_global.py wind_gaussian.py"
echo $src
git="/home/graphics/cgs/git/magics"

ext="py"
img_ext="png"
where='wind'
interpretor='python'

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
