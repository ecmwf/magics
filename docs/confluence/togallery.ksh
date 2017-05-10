#!/bin/ksh

python tojira.py

gallery="axis coastlines contour graph legend subpage symbols wind"
gallery="wind"

PYTHONPATH=..:$PYTHONPATH

for dir in $gallery
do
	cd $dir
	python tojira.py
    cd ..
done

cd gallery
python generateMagicsGallery.py
