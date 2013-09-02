#!/bin/bash

# First link the data 

for grib in `ls /home/graphics/cgx/metview/Shared_Graphics/magics/static_web/*.grib`
do
	ln -s $grib .
done


area

for name in `ls *deterministic.py` ; do
  echo "processing $name"
  basename $name _deterministic.py
  exname=`basename $name _deterministic.py`
  areas="Europe"
  for i in `ls $exname*` 
  do
	prefix=$exname"_"
	to=${i#$prefix}
	cp $i $to
	chmod +w $to
	if [ $to = "areas.cfg" ] ; then
		
		areas="Europe,South_Hemisphere,North_America"
	elif [ $to = "wave_areas.cfg" ] ; then
		areas="Global,Indian"
	fi

  done
  metpy deterministic.py area=$areas step=48 text=test 
  areas="Europe"
  mv ps.ps $exname.ps
  echo "saving  $exname.ps"
done

rm -f *.grib
