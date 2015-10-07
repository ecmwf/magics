if(Boost_FOUND)
  message( STATUS " Boost include : [${Boost_INCLUDE_DIRS}]" )
endif()

if( SWIG_FOUND AND NUMPY_FOUND )
  message( STATUS " Swig (${SWIG_VERSION}) : [${SWIG_EXECUTABLE}]" )
  message( STATUS " NumPy (${NUMPY_VERSION}) : [${NUMPY_INCLUDE_DIRS}]" )
endif()

if(PROJ4_FOUND)
  message( STATUS " Proj4 include : [${PROJ4_INCLUDE_DIRS}]" )
  message( STATUS "         libs  : [${PROJ4_LIBRARIES}]" )
endif()

if(PANGOCAIRO_FOUND)
  message( STATUS " PangoCairo include : [${PANGOCAIRO_INCLUDE_DIRS}]" )
  message( STATUS "              libs  : [${PANGOCAIRO_LIBRARIES}]" )
endif()

if(GRIB_API_FOUND)
  message( STATUS " GRIB_API include : [${GRIB_API_INCLUDE_DIRS}]" )
  message( STATUS "         libs     : [${GRIB_API_LIBRARIES}]" )
endif()

if(ODB_API_FOUND)
  message( STATUS " ODB_API include  : [${ODB_API_INCLUDE_DIRS}]" )
  message( STATUS "         libs     : [${ODB_API_LIBRARIES}]" )
endif()

if(LIBEMOS_FOUND)
  message( STATUS " LIBEMOS libs      : [${LIBEMOS_LIBRARIES}]" )
endif()

if(FORTRANLIBS_FOUND)
  message( STATUS " Fortran  libs    : [${FORTRAN_LIBRARIES}]" )
endif()

if(NETCDF_FOUND)
  message( STATUS " NetCDF include   : [${NETCDF_INCLUDE_DIRS}]" )
  message( STATUS "         libs     : [${NETCDF_LIBRARIES}]" )
endif()

if(QT_FOUND)
  message( STATUS " QT include       : [${QT_INCLUDE_DIR}]" )
  message( STATUS "         libs     : [${QT_LIBRARIES}]" )
endif()

if(EXPAT_FOUND)
  message( STATUS " EXpat include       : [${EXPAT_INCLUDE_DIR}]" )
  message( STATUS "         libs     : [${EXPAT_LIBRARIES}]" )
endif()

message( STATUS " Magics extra libs      : [${MAGICS_EXTRA_LIBRARIES}]" )
