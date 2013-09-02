
pgf90 graf02.F -o plot.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./plot.exe 
pgf90 bar.F -o bar.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./bar.exe 
