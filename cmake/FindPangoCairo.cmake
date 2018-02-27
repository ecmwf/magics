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

#ecbuild_debug_var( PC_LIBPANGOCAIRO_FOUND )
#ecbuild_debug_var( PC_LIBPANGOCAIRO_VERSION )
#ecbuild_debug_var( PC_LIBPANGOCAIRO_LIBRARIES )
#ecbuild_debug_var( PC_LIBPANGOCAIRO_LDFLAGS )
#ecbuild_debug_var( PC_LIBPANGOCAIRO_LDFLAGS_OTHER )
#ecbuild_debug_var( PC_LIBPANGOCAIRO_INCLUDE_DIRS )

if(PC_LIBPANGOCAIRO_FOUND)

    include(FindPackageHandleStandardArgs)
    # Handle the QUIET and REQUIRED arguments and set PANGOCAIRO_FOUND to TRUE
    # if all listed variables are TRUE
    # Note: capitalisation of the package name must be the same as in file name
    find_package_handle_standard_args( PangoCairo DEFAULT_MSG PC_LIBPANGOCAIRO_LIBRARIES PC_LIBPANGOCAIRO_INCLUDE_DIRS )
    set( PANGOCAIRO_VERSION ${PC_LIBPANGOCAIRO_VERSION} )
    set( PANGOCAIRO_LIBRARIES "${PC_LIBPANGOCAIRO_LDFLAGS} ${PC_LIBPANGOCAIRO_LDFLAGS_OTHER}" )
    set( PANGOCAIRO_INCLUDE_DIRS ${PC_LIBPANGOCAIRO_INCLUDE_DIRS} )

else()

    # this is to get magics compiling on mac with macbrew

    set(PANGO_VERSION 1.0)
    set(GLIB_VERSION 2.0)

    find_path( _PANGOCAIRO_INCLUDE_DIRS
        NAMES pango/pangocairo.h
        HINTS /usr/local/include PATH_SUFFIXES pango-${PANGO_VERSION})

    find_path( _CAIRO_INCLUDE_DIRS
        NAMES cairo.h
        HINTS /usr/local/include PATH_SUFFIXES cairo)

    find_path( _GLIB_INCLUDE_DIRS_1
        NAMES glib.h
        HINTS /usr/local/include PATH_SUFFIXES glib-${GLIB_VERSION})

    find_path( _GLIB_INCLUDE_DIRS_2
        NAMES glibconfig.h
        HINTS /usr/local/lib/glib-${GLIB_VERSION} PATH_SUFFIXES include)


    find_package(X11)

    set(PANGOCAIRO_INCLUDE_DIRS
        ${_PANGOCAIRO_INCLUDE_DIRS}
        ${_CAIRO_INCLUDE_DIRS}
        ${_GLIB_INCLUDE_DIRS_1}
        ${_GLIB_INCLUDE_DIRS_2}
        ${X11_INCLUDE_DIR}
    )

    find_library( _PANGOCAIRO_LIBRARIES NAMES pangocairo pangocairo-${PANGO_VERSION})
    find_library( _PANGO_LIBRARIES NAMES pango pango-${PANGO_VERSION})
    find_library( _CAIRO_LIBRARIES NAMES cairo)
    find_library( _GLIB_LIBRARIES NAMES glib-${GLIB_VERSION})

    set(PANGOCAIRO_LIBRARIES
        ${_PANGOCAIRO_LIBRARIES}
        ${_PANGO_LIBRARIES}
        ${_CAIRO_LIBRARIES}
        ${_GLIB_LIBRARIES}
        ${X11_LIBRARIES}
    )

    include(FindPackageHandleStandardArgs)

    # Handle the QUIET and REQUIRED arguments and set PANGOCAIRO_FOUND to TRUE
    # if all listed variables are TRUE
    # Note: capitalisation of the package name must be the same as in file name
    find_package_handle_standard_args( PangoCairo  DEFAULT_MSG
                                       _PANGOCAIRO_INCLUDE_DIRS
                                       _CAIRO_INCLUDE_DIRS
                                       _GLIB_INCLUDE_DIRS_1
                                       _GLIB_INCLUDE_DIRS_2
                                       X11_INCLUDE_DIR
                                       _PANGOCAIRO_LIBRARIES
                                       _PANGO_LIBRARIES
                                       _CAIRO_LIBRARIES
                                       _GLIB_LIBRARIES
                                       X11_LIBRARIES )

endif()

mark_as_advanced( PANGOCAIRO_INCLUDE_DIRS PANGOCAIRO_LIBRARIES )
