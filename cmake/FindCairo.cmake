# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find the cairo library
# Once done this will define
#
# CAIRO_FOUND - system has cairo
# CAIRO_INCLUDE_DIRS - the cairo include directory
# CAIRO_LIBRARIES - Link these to use cairo
#
# Define CAIRO_MIN_VERSION for which version desired.


if( NOT DEFINED CAIRO_PATH AND NOT "$ENV{CAIRO_PATH}" STREQUAL "" )
    set( APPEND CAIRO_PATH "$ENV{CAIRO_PATH}" )
endif()

if( NOT DEFINED CAIRO_PATH )

    include(FindPkgConfig)

    if(Cairo_FIND_REQUIRED)
        set(_pkgconfig_REQUIRED "REQUIRED")
    else()
        set(_pkgconfig_REQUIRED "")
    endif()

    if(CAIRO_MIN_VERSION)
        pkg_check_modules(PKCAIRO ${_pkgconfig_REQUIRED} cairo>=${CAIRO_MIN_VERSION})
    else()
        pkg_check_modules(PKCAIRO ${_pkgconfig_REQUIRED} cairo)
    endif()

    if( PKG_CONFIG_FOUND AND PKCAIRO_FOUND )

        find_path(CAIRO_INCLUDE_DIR cairo.h HINTS ${PKCAIRO_INCLUDEDIR} ${PKCAIRO_INCLUDE_DIRS} PATH_SUFFIXES cairo NO_DEFAULT_PATH )
        find_library(CAIRO_LIBRARY  cairo   HINTS ${PKCAIRO_LIBDIR}     ${PKCAIRO_LIBRARY_DIRS} PATH_SUFFIXES cairo NO_DEFAULT_PATH )

    endif()

else()

    find_path(CAIRO_INCLUDE_DIR cairo.h PATHS ${CAIRO_PATH}/include PATH_SUFFIXES cairo NO_DEFAULT_PATH )
    find_library(CAIRO_LIBRARY  cairo   PATHS ${CAIRO_PATH}/lib     PATH_SUFFIXES cairo NO_DEFAULT_PATH )

endif()

find_path(CAIRO_INCLUDE_DIR cairo.h PATH_SUFFIXES cairo )
find_library( CAIRO_LIBRARY cairo   PATH_SUFFIXES cairo )

set( CAIRO_LIBRARIES    ${CAIRO_LIBRARY} )
set( CAIRO_INCLUDE_DIRS ${CAIRO_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIET and REQUIRED arguments and set CAIRO_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args(Cairo  DEFAULT_MSG
                                  CAIRO_LIBRARY CAIRO_INCLUDE_DIR)

mark_as_advanced( CAIRO_INCLUDE_DIR CAIRO_LIBRARY )
