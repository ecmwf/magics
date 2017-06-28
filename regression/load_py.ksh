#!/bin/ksh


set +x

versions="2.33.0" 

ext="py"
img_ext="png"
where='gallery'
interpretor='python'
src='axis5'

dir=`pwd`
name=`basename $dir`



for s in $src
do
for v in $versions
do
  module load Magics/$v
  echo "magics/reference/$v/$where"
#  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor
  ../upload.py $v ./$src.py $src.png magics/reference/$v/$where -i $interpretor

done
done
