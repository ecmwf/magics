# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find PangoCairo

# Output:
#   PANGOCAIRO_FOUND
#   PANGOCAIRO_LIBRARIES
#   PANGOCAIRO_INCLUDE_DIRS


find_package(PkgConfig)

pkg_check_modules(PC_LIBPANGOCAIRO QUIET pangocairo)

if(PC_LIBPANGOCAIRO_FOUND)

    include(FindPackageHandleStandardArgs)
    # Handle the QUIET and REQUIRED arguments and set PANGOCAIRO_FOUND to TRUE
    # if all listed variables are TRUE
    # Note: capitalisation of the package name must be the same as in file name
    find_package_handle_standard_args( PangoCairo DEFAULT_MSG PC_LIBPANGOCAIRO_LIBRARIES PC_LIBPANGOCAIRO_INCLUDE_DIRS )
    set( PANGOCAIRO_VERSION ${PC_LIBPANGOCAIRO_VERSION} )
    # To allow for Frameworks under MacOS which are given in two separate strings see ECBUILD-360
    #set( PANGOCAIRO_LIBRARIES "${PC_LIBPANGOCAIRO_LDFLAGS} ${PC_LIBPANGOCAIRO_LDFLAGS_OTHER}" )
    string (REPLACE ";" " " PANGOCAIRO_LIBRARIES "${PC_LIBPANGOCAIRO_LDFLAGS} ${PC_LIBPANGOCAIRO_LDFLAGS_OTHER}")
    set( PANGOCAIRO_INCLUDE_DIRS ${PC_LIBPANGOCAIRO_INCLUDE_DIRS} )
else()
    message(ERROR "PangoCairo not found!")
endif()

mark_as_advanced( PANGOCAIRO_INCLUDE_DIRS PANGOCAIRO_LIBRARIES )
