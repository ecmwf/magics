
pgf90 -byteswapio LAGUNA.f plot.f -o plot.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED

./plot.exe 
