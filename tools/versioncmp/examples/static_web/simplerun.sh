#!/bin/bash

# First link the data 

for grib in `ls /home/graphics/cgx/metview/Shared_Graphics/magics/static_web/*.grib`
do
	ln -s $grib .
done



for name in $1
do
  echo "processing $name"
  areas="Europe"
  for i in `ls $name*` 
  do
	prefix=$name"_"
	to=${i#$prefix}
	cp $i $to
	chmod +w $to
	if [ $to = "areas.cfg" ] ; then
		
		areas="Europe,South_Hemisphere,North_America"
	elif [ $to = "wave_areas.cfg" ] ; then
		areas="Global,Indian"
	fi

  done
  echo " metpy deterministic.py area=$areas step=48 text=test"
  metpy deterministic.py area=$areas step=48 text=test 
  areas="Europe"
  mv ps.ps $name.ps
  echo "saving  $name.ps"
done

#rm -f *.grib
