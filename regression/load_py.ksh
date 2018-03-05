#!/bin/ksh


set +x

versions="3.0.0" 

ext="py"
img_ext="png"
where='gallery'
interpretor='python'
src='palette2'

dir=`pwd`
name=`basename $dir`



for s in $src
do
for v in $versions
do
  module load Magics/$v
  echo "magics/reference/$v/$where"
#  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor
  /tmp/cgs/git/magics-clean/regression/upload.py $v ./$src.py $src.png magics/reference/$v/$where -i $interpretor

done
done
