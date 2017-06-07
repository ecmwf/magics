#!/bin/ksh


set +x

versions="2.32.0" 

ext="py"
img_ext="png"
where='web'
interpretor='python'
src='ptypegram'

dir=`pwd`
name=`basename $dir`



for s in $src
do
for v in $versions
do
  module unload Magics
  #module load Magics/$v
  module load Magics/debug
  echo "magics/reference/$v/$where"
#  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor
  ../upload.py $v ./$src.py $src.png magics/reference/$v/$where -i $interpretor

done
done
