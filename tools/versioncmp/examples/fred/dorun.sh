export GRIB_API_VERSION=1.9.9
pgf90 -byteswapio plot.F90 -o plot.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./plot.exe
