#!/bin/bash

rm -fr diff?
mkdir -p diff0 diff1 diff2

set -ex

if [[ ! -f era5.grib ]]
then
    python3 ./get-era5.py
    cat era5-pl.grib era5-sfc.grib > era5.grib
    rm -f era5-pl.grib era5-sfc.grib
fi

export MAGPLUS_HOME=../..

for i in $(seq $(grib_count era5.grib))
do
if [[ $i -eq 28 || $i -eq 31 || $i == 207 ]]
then
continue
fi
p=$i
climetlab=$(printf "diff0/era5-%04d-c.png" $i)
original=$(printf "diff0/era5-%04d-o.png" $i)
ecmwf=$(printf "diff0/era5-%04d-e.png" $i)

if [[ -f $original ]]
then
continue
fi

cat<<EOF >tmp.py
from Magics.macro import *
plot(output(
        output_formats=["png"],
        output_name="original",
        page_id_line=False,
        output_name_first_page_number=False),
    mgrib(grib_field_position=$p,
    grib_input_file_name="era5.grib"),
    mcont(contour_automatic_setting="ecmwf",legend=False,contour_threads=1))
EOF

python3 tmp.py
mv style original.style

cat<<EOF >tmp.yaml
plot:
    - output:
        output_formats:
            - png
        output_name: ecmwf
        output_name_first_page_number: false
        page_id_line: false
    - mgrib:
        grib_field_position: $p
        grib_input_file_name: era5.grib
    - mcont:
        contour_automatic_setting: ecmwf
        legend: false
        contour_threads: 1
EOF


~/build/magics-b8raoult/bin/magics tmp.yaml
mv style ecmwf.style

cat<<EOF >tmp.yaml
plot:
    - output:
        output_formats:
            - png
        output_name: climetlab
        output_name_first_page_number: false
        page_id_line: false
    - mgrib:
        grib_field_position: $p
        grib_input_file_name: era5.grib
    - mcont:
        contour_automatic_setting: climetlab
        legend: false
        contour_threads: 1
EOF

~/build/magics-b8raoult/bin/magics tmp.yaml
mv style climetlab.style

diff original.style ecmwf.style || (cp original.style $original.style; cp ecmwf.style $ecmwf.style)
diff climetlab.style ecmwf.style || (cp climetlab.style $climetlab.style; cp ecmwf.style $ecmwf.style)

mv climetlab.png $climetlab
mv original.png $original
mv ecmwf.png $ecmwf

set +e
perceptualdiff $ecmwf $original
status=$?
set -e

if [[ $status -ne 0 ]]
then
    diff="$(basename $original .png).ppm"
    perceptualdiff -output $diff $ecmwf $original || true
    mv $ecmwf $original $climetlab diff1/
    mv $diff diff1/
    continue
fi

set +e
perceptualdiff $ecmwf $climetlab
status=$?
set -e

if [[ $status -ne 0 ]]
then
    diff="$(basename $climetlab .png).ppm"
    perceptualdiff -output $diff $ecmwf $climetlab | true
    mv $ecmwf $original $climetlab diff2/
    mv $diff diff2/
    continue
fi



done
