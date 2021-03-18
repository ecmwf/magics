#!/bin/ksh

versions="current++" 
versions="current++ new++" 
src=`ls *json`
dir=`pwd`
name=`basename $dir`


for sf in $src 
do
  s=`basename $sf .json`
for v in $versions
do
    x="add_magics_test(NAME $s SOURCES $s.json $s.png DATA msl.grib uv.grib THRESHOLD 50 JSON)"
	version=`/usr/local/apps/Magics/$v/bin/magics-config --version`
    ../upload.py $version ./$sf $s.png magics/reference/$version/proj4 /usr/local/apps/Magics/$v/bin/magjson
done
done
