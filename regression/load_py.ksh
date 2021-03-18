#!/bin/ksh


set +x

versions="3.0.1" 

ext="py"
img_ext="png"
where='proj4'
interpretor='python'
src='tpers'

dir=`pwd`
name=`basename $dir`



for s in $src
do
for v in $versions
do
  module swap Magics/$v
  echo "magics/reference/$v/$where"
#  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor
  /tmp/cgs/git/magics-develop/regression/upload.py $v ./$src.py $src.png magics/reference/$v/$where -i $interpretor

done
done
