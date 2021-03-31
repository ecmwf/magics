# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


if(PROJ_FOUND)
  message( STATUS " Proj include : [${PROJ_INCLUDE_DIRS}]" )
  message( STATUS "        libs  : [${PROJ_LIBRARIES}]" )
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
