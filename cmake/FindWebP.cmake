# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find the WebP includes and library
# This module defines
#
#  WEBP_FOUND         - System has WebP
#  WEBP_INCLUDE_DIRS  - the WebP include directories
#  WEBP_LIBRARIES     - the libraries needed to use WebP
#
# The following paths will be searched with priority if set in CMake or env
#
#  WEBP_DIR   - root folder of the WebP installation
#  WEBP_PATH  - root folder of the WebP installation
#
# Define WEBP_MIN_VERSION for which version desired.

if( NOT WEBP_PATH )
    if ( NOT "$ENV{WEBP_PATH}" STREQUAL "" )
        set( WEBP_PATH "$ENV{WEBP_PATH}" )
    elseif ( NOT "$ENV{WEBP_DIR}" STREQUAL "" )
        set( WEBP_PATH "$ENV{WEBP_DIR}" )
    endif()
endif()

if( NOT WEBP_PATH )

    include(FindPkgConfig)

    if(WEBP_MIN_VERSION)
        pkg_check_modules(PKWEBP ${_pkgconfig_REQUIRED} QUIET WEBP>=${WEBP_MIN_VERSION})
    else()
        pkg_check_modules(PKWEBP ${_pkgconfig_REQUIRED} QUIET WEBP)
    endif()

    if( PKG_CONFIG_FOUND AND PKWEBP_FOUND )

        find_path(WEBP_INCLUDE_DIR encode.h HINTS ${PKWEBP_INCLUDEDIR} ${PKWEBP_INCLUDE_DIRS} PATH_SUFFIXES WEBP NO_DEFAULT_PATH )
        find_library(WEBP_LIBRARY  webp     HINTS ${PKWEBP_LIBDIR}     ${PKWEBP_LIBRARY_DIRS} PATH_SUFFIXES WEBP NO_DEFAULT_PATH )

    endif()

endif()

if( WEBP_PATH )

    find_path(WEBP_INCLUDE_DIR NAMES encode.h PATHS ${WEBP_PATH} ${WEBP_PATH}/include PATH_SUFFIXES WEBP NO_DEFAULT_PATH )
    find_library(WEBP_LIBRARY  NAMES webp   PATHS ${WEBP_PATH} ${WEBP_PATH}/lib     PATH_SUFFIXES WEBP NO_DEFAULT_PATH )

endif()

find_path(WEBP_INCLUDE_DIR NAMES encode.h PATHS PATH_SUFFIXES WEBP )
find_library( WEBP_LIBRARY NAMES webp     PATHS PATH_SUFFIXES WEBP )


# handle the QUIETLY and REQUIRED arguments and set GRIBAPI_FOUND
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(WEBP  DEFAULT_MSG
                                  WEBP_LIBRARY WEBP_INCLUDE_DIR)

set( WEBP_LIBRARIES    ${WEBP_LIBRARY} )
set( WEBP_INCLUDE_DIRS ${WEBP_INCLUDE_DIR} )

mark_as_advanced( WEBP_INCLUDE_DIR WEBP_LIBRARY )
