
pgf90 import_1.F -o plot.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./plot.exe 


python import.py


