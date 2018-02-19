# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find the GeoTIFF includes and library
# This module defines
#
#  GEOTIFF_FOUND         - System has GeoTIFF
#  GEOTIFF_INCLUDE_DIRS  - the GeoTIFF include directories
#  GEOTIFF_LIBRARIES     - the libraries needed to use GeoTIFF
#
# The following paths will be searched with priority if set in CMake or env
#
#  GEOTIFF_DIR   - root folder of the GeoTIFF installation
#  GEOTIFF_PATH  - root folder of the GeoTIFF installation

find_path( GEOTIFF_INCLUDE_DIR geotiff.h
           PATHS ${GEOTIFF_PATH} ENV GEOTIFF_PATH
                 ${GEOTIFF_DIR}  ENV GEOTIFF_DIR
           PATH_SUFFIXES include include/libgeotiff
           NO_DEFAULT_PATH )
find_path( GEOTIFF_INCLUDE_DIR  openjpeg.h
           PATH_SUFFIXES include include/libgeotiff )

find_library( GEOTIFF_LIBRARY NAMES geotiff
              PATHS ${GEOTIFF_PATH} ENV GEOTIFF_PATH
                    ${GEOTIFF_DIR}  ENV GEOTIFF_DIR
              PATH_SUFFIXES lib lib64
              NO_DEFAULT_PATH )
find_library( GEOTIFF_LIBRARY NAMES geotiff )

set( GEOTIFF_LIBRARIES    ${GEOTIFF_LIBRARY} )
set( GEOTIFF_INCLUDE_DIRS ${GEOTIFF_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set GEOTIFF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args( GeoTIFF DEFAULT_MSG
                                   GEOTIFF_LIBRARY GEOTIFF_INCLUDE_DIR )

mark_as_advanced( GEOTIFF_INCLUDE_DIR GEOTIFF_LIBRARY )
