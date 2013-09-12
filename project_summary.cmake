if(Boost_FOUND)
  message( STATUS " Boost include : [${Boost_INCLUDE_DIRS}]" )
endif()

if(PROJ4_FOUND)
  message( STATUS " Proj4 include : [${PROJ4_INCLUDE_DIRS}]" )
  message( STATUS "         libs  : [${PROJ4_LIBRARIES}]" )
endif()

if(CAIRO_FOUND)
  message( STATUS " Cairo include : [${CAIRO_INCLUDE_DIRS}]" )
  message( STATUS "         libs  : [${CAIRO_LIBRARIES}]" )
endif()

if(PANGO_FOUND)
  message( STATUS " Pango version : [${PANGO_VERSION}]" )
  message( STATUS "       include : [${PANGO_INCLUDE_DIRS}]" )
  message( STATUS "         libs  : [${PANGO_LIBRARIES}]" )
endif()

if(GRIB_API_FOUND)
  message( STATUS " GRIB_API include : [${GRIB_API_INCLUDE_DIRS}]" )
  message( STATUS "         libs     : [${GRIB_API_LIBRARIES}]" )
endif()

if(ODB_API_FOUND)
  message( STATUS " ODB_API include  : [${ODB_API_INCLUDE_DIRS}]" )
  message( STATUS "         libs     : [${ODB_API_LIBRARIES}]" )
endif()

if(EMOS_FOUND)
  message( STATUS " EMOS   libs      : [${EMOS_LIBRARIES}]" )
endif()

if(NETCDF_FOUND)
  message( STATUS " NetCDF include   : [${NETCDF_INCLUDE_DIRS}]" )
  message( STATUS "         libs     : [${NETCDF_LIBRARIES}]" )
endif()

if(QT_FOUND)
  message( STATUS " QT include       : [${QT_INCLUDE_DIR}]" )
  message( STATUS "         libs     : [${QT_LIBRARIES}]" )
endif()

