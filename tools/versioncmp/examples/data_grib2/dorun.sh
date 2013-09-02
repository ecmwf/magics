
# Comment :
# This file has a very low resolution.. From2.15 it is considered not global
# ie : linera method applied
#

pgf90 data_grib2.F -o plot.exe $GRIB_API_LIB $GRIB_API_INCLUDE $MAGPLUSLIB_SHARED
./plot.exe 
