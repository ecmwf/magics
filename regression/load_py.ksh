#!/bin/ksh


set +x

versions="2.24.0" 
src="axis2.py"
echo $src

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
  ../upload.py $v ./$sf $s.$img_ext magics/reference/$v/$where -i $interpretor

done
done
