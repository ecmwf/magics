# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find the proj library
# Once done this will define
#
# PROJ_FOUND - system has proj
# PROJ_INCLUDE_DIRS - the proj include directory
# PROJ_LIBRARIES - Link these to use proj
#
# Define PROJ_MIN_VERSION for which version desired.

if( NOT PROJ_PATH )
    if ( NOT "$ENV{PROJ_PATH}" STREQUAL "" )
        set( PROJ_PATH "$ENV{PROJ_PATH}" )
    elseif ( NOT "$ENV{PROJ_DIR}" STREQUAL "" )
        set( PROJ_PATH "$ENV{PROJ_DIR}" )
    endif()
endif()

if( NOT PROJ_PATH )

    include(FindPkgConfig)

    if(PROJ_MIN_VERSION)
        pkg_check_modules(PKPROJ ${_pkgconfig_REQUIRED} QUIET PROJ>=${PROJ_MIN_VERSION})
    else()
        pkg_check_modules(PKPROJ ${_pkgconfig_REQUIRED} QUIET PROJ)
    endif()

    if( PKG_CONFIG_FOUND AND PKPROJ_FOUND )

        find_path(PROJ_INCLUDE_DIR proj.h HINTS ${PKPROJ_INCLUDEDIR} ${PKPROJ_INCLUDE_DIRS} PATH_SUFFIXES PROJ NO_DEFAULT_PATH )
        find_library(PROJ_LIBRARY  proj   HINTS ${PKPROJ_LIBDIR}     ${PKPROJ_LIBRARY_DIRS} PATH_SUFFIXES PROJ NO_DEFAULT_PATH )

    endif()

endif()

if( PROJ_PATH )

    find_path(PROJ_INCLUDE_DIR NAMES proj.h PATHS ${PROJ_PATH} ${PROJ_PATH}/include PATH_SUFFIXES PROJ NO_DEFAULT_PATH )
    find_library(PROJ_LIBRARY  NAMES proj   PATHS ${PROJ_PATH} ${PROJ_PATH}/lib     PATH_SUFFIXES PROJ NO_DEFAULT_PATH )

endif()

find_path(PROJ_INCLUDE_DIR NAMES proj.h PATHS PATH_SUFFIXES PROJ )
find_library( PROJ_LIBRARY NAMES proj   PATHS PATH_SUFFIXES PROJ )


# handle the QUIETLY and REQUIRED arguments and set GRIBAPI_FOUND
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PROJ  DEFAULT_MSG
                                  PROJ_LIBRARY PROJ_INCLUDE_DIR)

set( PROJ_LIBRARIES    ${PROJ_LIBRARY} )
set( PROJ_INCLUDE_DIRS ${PROJ_INCLUDE_DIR} )

mark_as_advanced( PROJ_INCLUDE_DIR PROJ_LIBRARY )
