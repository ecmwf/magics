#!/bin/ksh


set +x

versions="2.26.0" 
src="preciptype.py"
src="handling_missing1.py handling_missing2.py handling_missing3.py handling_missing4.py "
echo $src

ext="py"
img_ext="png"
where='web'
interpretor='python'

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
  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor

done
done
