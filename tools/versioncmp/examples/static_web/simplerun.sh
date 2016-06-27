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
