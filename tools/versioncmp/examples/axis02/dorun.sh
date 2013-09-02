
pgf90 axis02.F -o plot.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./plot.exe 

pgf90 automatic_axis.F -o automatic_axis.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./automatic_axis.exe 
