#!/bin/bash
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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
