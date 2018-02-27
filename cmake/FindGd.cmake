# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find the GD library
# Once done this will define
#
# GD_FOUND - system has GD
# GD_INCLUDE_DIRS - the GD include directory
# GD_LIBRARIES - Link these to use GD
#
# Define GD_MIN_VERSION for which version desired.


if( NOT DEFINED GD_PATH AND NOT "$ENV{GD_PATH}" STREQUAL "" )
    set( APPEND GD_PATH "$ENV{GD_PATH}" )
endif()

if( NOT DEFINED GD_PATH )

    include(FindPkgConfig)

    if(GD_FIND_REQUIRED)
        set(_pkgconfig_REQUIRED "REQUIRED")
    else()
        set(_pkgconfig_REQUIRED "")
    endif()

    if(GD_MIN_VERSION)
        pkg_check_modules(PKGD ${_pkgconfig_REQUIRED} GD>=${GD_MIN_VERSION})
    else()
        pkg_check_modules(PKGD ${_pkgconfig_REQUIRED} GD)
    endif()

    if( PKG_CONFIG_FOUND AND PKGD_FOUND )

        find_path(GD_INCLUDE_DIR gd.h HINTS ${PKGD_INCLUDEDIR} ${PKGD_INCLUDE_DIRS} PATH_SUFFIXES GD NO_DEFAULT_PATH )
        find_library(GD_LIBRARY  gd   HINTS ${PKGD_LIBDIR}     ${PKGD_LIBRARY_DIRS} PATH_SUFFIXES GD NO_DEFAULT_PATH )

    endif()

else()

    find_path(GD_INCLUDE_DIR gd.h PATHS ${GD_PATH}/include PATH_SUFFIXES GD NO_DEFAULT_PATH )
    find_library(GD_LIBRARY  gd   PATHS ${GD_PATH}/lib     PATH_SUFFIXES GD NO_DEFAULT_PATH )

endif()

find_path(GD_INCLUDE_DIR gd.h PATH_SUFFIXES GD )
find_library( GD_LIBRARY gd   PATH_SUFFIXES GD )

set( GD_LIBRARIES    ${GD_LIBRARY} )
set( GD_INCLUDE_DIRS ${GD_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIET and REQUIRED arguments and set GD_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args(GD  DEFAULT_MSG
                                  GD_LIBRARY GD_INCLUDE_DIR)

mark_as_advanced( GD_INCLUDE_DIR GD_LIBRARY )
