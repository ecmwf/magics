#!/bin/ksh


set +x

versions="2.30.0" 

ext="py"
img_ext="png"
where='efas'
interpretor='python'
src='efas_plot.py'

dir=`pwd`
name=`basename $dir`



for sf in $src 
do
    s=`basename $sf .$ext`

for v in $versions
do
  module unload Magics
  module load Magics/$v
  echo "magics/reference/$v/$where"
#  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor
  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor

done
done
