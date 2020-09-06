############################################################################################
# cmake options:
#
#       -DCMAKE_BUILD_TYPE=Debug|RelWithDebInfo|Release|Production
#
#       -DCMAKE_MODULE_PATH=/path/to/ecbuild/cmake
#
#       -DCMAKE_C_COMPILER=gcc
#
#       -DCMAKE_PREFIX_PATH=/path/to/jasper:/path/to/any/package/out/of/place
#

cmake_minimum_required( VERSION 3.6.0 FATAL_ERROR )

project( magics VERSION 4.4.5 LANGUAGES CXX )

# make sure that the header files are installed into include/magics
# note that this needs to be done before ecbuild_declare_project()
# to ensure that the ecbuild header files are also put there
# note also that we need to CACHE this so that ecbuild_declare_project() does not overwrite it
set(INSTALL_INCLUDE_DIR include/magics CACHE PATH "Magics installation directory for header files")

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include(ecbuild_system NO_POLICY_SCOPE)


set (CMAKE_CXX_STANDARD 14)
set (CMAKE_C_STANDARD 99)

###############################################################################
# some variables of this project

ecbuild_find_package( NAME eccodes VERSION 2.9.2 REQUIRED
                      FAILURE_MSG "ecCodes is required in order to build Magics" )

set( HAVE_GRIB 1 )     ##### remove both
set( HAVE_BUFR 1 )
                    
ecbuild_add_option( FEATURE ODB
                    DEFAULT OFF
                    DESCRIPTION "ODB support - to be deprecated"
                    REQUIRED_PACKAGES "NAME odb_api VERSION 0.10.2")

#ecbuild_add_option( FEATURE CAIRO
#                    DEFAULT ON
#                    DESCRIPTION "cairo support[png/jpeg]"
#                    REQUIRED_PACKAGES PangoCairo)

option( ENABLE_CAIRO "cairo support[png/jpeg]" ON )
if( ENABLE_CAIRO )
  find_package(PangoCairo REQUIRED)
  if( NOT PANGOCAIRO_FOUND)
    message(FATAL_ERROR "Cairo support (PNG & PDF output) requested, but requirements were not found - can be disabled with -DENABLE_CAIRO=OFF")
  endif()
  set(HAVE_CAIRO ON)
  ecbuild_enable_feature( CAIRO )
else()
  ecbuild_disable_feature( CAIRO )
endif()

ecbuild_add_option( FEATURE GEOTIFF
                    DEFAULT OFF
                    DESCRIPTION "geotiff support [implies cairo] -  work on progress"
                    CONDITION HAVE_CAIRO
                    REQUIRED_PACKAGES GeoTIFF )

ecbuild_add_option( FEATURE NETCDF
                    DEFAULT ON
                    DESCRIPTION "enable netcdf support"
                    REQUIRED_PACKAGES "NetCDF 4 COMPONENTS C" )

find_package(PythonInterp REQUIRED)
find_package(Jinja2 REQUIRED)

######## TO BE REMOVED ######################  if Fortran compiler run tests
ecbuild_add_option( FEATURE FORTRAN
                    DEFAULT OFF
                    DESCRIPTION "enable Fortran interface" )
set( HAVE_FORTRAN 0 )

ecbuild_add_option( FEATURE METVIEW
                    DEFAULT OFF
                    DESCRIPTION "enable Metview interface" )

ecbuild_add_option( FEATURE METVIEW_NO_QT
                    DEFAULT OFF
                    DESCRIPTION "enable Metview interface without Qt" )

ecbuild_add_option( FEATURE EFAS
                    DEFAULT OFF
                    DESCRIPTION "enable EFAS coastlines and catchments" )


set( MAGICS_NAME          "Magics" )
set( MAGICS_EXCEPTION     "ON" )    ##################### REMOVE 
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )    ##### Stephan will check


###############################################################################
### Metview and Qt
ecbuild_info("TESTING for Metview and Qt ...")

if(HAVE_METVIEW AND HAVE_METVIEW_NO_QT)
    ecbuild_critical("Do not set both HAVE_METVIEW and HAVE_METVIEW_NO_QT - only set one. You may have to remove your CMakeCache.txt to clear these settings.")
endif()

set( qt no )
set( metview no )

if( HAVE_METVIEW_NO_QT )
	set( metview yes )
	unset(MAGICS_ONLY)
endif()

if( HAVE_METVIEW )
    set ( metview yes)
    ecbuild_info("TESTING for Qt5 ...")
    unset(MAGICS_ONLY)

    find_package(Qt5Widgets REQUIRED)
    if( Qt5Widgets_FOUND )
        ecbuild_info("Qt5 was found ... ${Qt5Widgets_VERSION_STRING}")
        include_directories(${Qt5Widgets_INCLUDE_DIRS})
        set( MAGICS_QT 1)
        set( MAGICS_QT5 1)
        set( qt yes)
        add_definitions( -DMAGICS_QT5 )
    else()
        ecbuild_critical("Qt5 was NOT found, but it is necessary if METVIEW is enabled ...")
    endif()
endif()

ecbuild_declare_project()

###############################################################################
# find extra packages

set( CMAKE_THREAD_PREFER_PTHREAD 1 )
find_package( Threads )

if( EC_OS_NAME MATCHES "windows" )
    find_library( PTHREAD_LIB pthread PATHS ENV PATH )
    list( APPEND MAGICS_EXTRA_LIBRARIES ${PTHREAD_LIB} )
endif()

# adding NO_MODULE means that if we don't find it, CMake will not generate
# strange error messages about not being able to find the FindProj4.cmake file.
# this should find proj if it has been installed via CMake, but if it has been
# installed via autotools, then we will need to try another method
find_package( PROJ4 6.1.0 NO_MODULE )
if (NOT PROJ4_FOUND)
    find_package(Proj 6.1.0 REQUIRED)
    if (NOT PROJ_FOUND)
        message(FATAL_ERROR "Package 'proj' was not found - at least version 6.1.0 required.")
    endif()
endif()

find_package( EXPAT REQUIRED )   # for MagML

if(APPLE)
  find_package( zlib REQUIRED )       # dependency of minizip - KMZ support
endif(APPLE)

###############################################################################
# Windows compiler options

if( EC_OS_NAME MATCHES "windows" )
    # Suppress compliler warnings
    # Suppress warnings about using 'insecure' functions. Fixing this would require changes all over
    # the codebase which would damage portability.
    ecbuild_add_c_flags("/D_CRT_SECURE_NO_WARNINGS")
    ecbuild_add_cxx_flags("/D_CRT_SECURE_NO_WARNINGS")
    # Suppress warnings about using well-known C functions.
    ecbuild_add_c_flags("/D_CRT_NONSTDC_NO_DEPRECATE")
    ecbuild_add_cxx_flags("/D_CRT_NONSTDC_NO_DEPRECATE")
    # Suppress C4267: warns about possible loss of data when converting 'size_t' to 'int'.
    ecbuild_add_c_flags("/wd4267")
    ecbuild_add_cxx_flags("/wd4267")
    # Suppress C4800: forcing value to bool 'true' or 'false'
    ecbuild_add_cxx_flags("/wd4800")

    # Prevent fatal error C1128
    ecbuild_add_cxx_flags("/bigobj")
endif()

###############################################################################
# contents

set( MAGICS_TPLS eccodes EXPAT NetCDF PROJ4 Proj zlib )

set( MAGICS_INCLUDE_DIRS
     ${CMAKE_CURRENT_BINARY_DIR}/src
     ${CMAKE_CURRENT_BINARY_DIR}/src/params
     ${CMAKE_CURRENT_SOURCE_DIR}/src
     ${CMAKE_CURRENT_SOURCE_DIR}/src/common
     ${CMAKE_CURRENT_SOURCE_DIR}/src/basic
     ${CMAKE_CURRENT_SOURCE_DIR}/src/clipper
     ${CMAKE_CURRENT_SOURCE_DIR}/src/web
     ${CMAKE_CURRENT_SOURCE_DIR}/src/visualisers
     ${CMAKE_CURRENT_SOURCE_DIR}/src/drivers
     ${CMAKE_CURRENT_SOURCE_DIR}/src/drivers/MgQ
     ${CMAKE_CURRENT_SOURCE_DIR}/src/eckit_readers
     ${CMAKE_CURRENT_SOURCE_DIR}/src/decoders
     ${CMAKE_CURRENT_SOURCE_DIR}/src/terralib/kernel
     ${CMAKE_CURRENT_SOURCE_DIR}/src/libMagWrapper)

if( HAVE_CAIRO )
  list( APPEND MAGICS_TPLS PangoCairo)
  list( APPEND MAGICS_INCLUDE_DIRS ${PANGOCAIRO_INCLUDE_DIRS})
endif()

if( HAVE_ODB )    ### deprecated!!!
  list( APPEND MAGICS_TPLS odb_api )
  list( APPEND MAGICS_INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/src/oda
        ${ODB_API_INCLUDE_DIRS}
		${ECKIT_INCLUDE_DIRS} )
endif()

set( MAGICS_LIBRARIES MagPlus )

foreach( _tpl ${MAGICS_TPLS} )
    string( TOUPPER ${_tpl} TPL )

    if( REPLACE_TPL_ABSOLUTE_PATHS )
        # replace TPL absolute paths with their library names
        # this helps make Magics relocatable
        set( _TMP "" )

        string(REPLACE " " ";" LIB_LIST "${${TPL}_LIBRARIES}") # lists of libs can have spaces; replace with semicolon
        foreach( _lib ${LIB_LIST} )
            get_filename_component( _lib_name ${_lib} NAME_WE )
            string( REGEX REPLACE "^lib" "" _name ${_lib_name} )
            if( NOT ${_name} STREQUAL ${_lib_name} )
              list( APPEND _TMP "-l${_name}" )
            else()
              list( APPEND _TMP "${_lib}" )
          endif()
        endforeach()
        list(REMOVE_DUPLICATES  _TMP)
        list(REMOVE_ITEM _TMP debug)
        list(REMOVE_ITEM _TMP optimized)

        set( ${TPL}_LIBRARIES ${_TMP} )
        set( _TMP "" )
    endif()

    list( APPEND MAGICS_EXTRA_DEFINITIONS ${${TPL}_DEFINITIONS}  )
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS ${${TPL}_INCLUDE_DIRS} )
    string(REGEX REPLACE "[ ]+$" "" ${TPL}_LIBRARIES "${${TPL}_LIBRARIES}") # strips leading whitespaces
    string(REGEX REPLACE "^[ ]+" "" ${TPL}_LIBRARIES "${${TPL}_LIBRARIES}") # strips trailing whitespaces
    list( APPEND MAGICS_EXTRA_LIBRARIES ${${TPL}_LIBRARIES} )
    ecbuild_info("${TPL}_LIBRARIES [${${TPL}_LIBRARIES}]") # add this to confirm that indeed it was cleaned up
endforeach()

if( MAGICS_QT )
  list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${Qt5Widgets_INCLUDE_DIR} )
  list( APPEND MAGICS_EXTRA_LIBRARIES     ${Qt5Widgets_LIBRARIES} )

  if( WITH_QT_DEBUG )
    list( APPEND MAGICS_EXTRA_DEFINITIONS QT_NO_DEBUG_OUTPUT )
  endif()
endif()

if( HAVE_GEOTIFF )
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${GEOTIFF_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES  ${GEOTIFF_LIBRARY} )
  list( APPEND MAGICS_EXTRA_DEFINITIONS HAVE_GEOTIFF )
endif()

list( APPEND MAGICS_EXTRA_LIBRARIES     ${CMAKE_THREAD_LIBS_INIT} )

if( EC_OS_NAME MATCHES "windows" )
    # We need to link to ws2_32.lib to avoid an unresolved external (gethostname)
    # It's part of the Windows SDK so no need to search for it.
    list( APPEND MAGICS_EXTRA_LIBRARIES ws2_32 )
    # We need to find zlib.lib
    find_library( ZLIB_LIB zlib PATHS ENV PATH )
    list( APPEND MAGICS_EXTRA_LIBRARIES ${ZLIB_LIB} )
endif()

ecbuild_debug("MAGICS_EXTRA_DEFINITIONS  => ${MAGICS_EXTRA_DEFINITIONS}")
ecbuild_debug("MAGICS_EXTRA_INCLUDE_DIRS => ${MAGICS_EXTRA_INCLUDE_DIRS}")
ecbuild_debug("MAGICS_EXTRA_LIBRARIES    => ${MAGICS_EXTRA_LIBRARIES}")

# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS "${ECKIT_DEFINITIONS}" )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

include_directories( ${MAGICS_INCLUDE_DIRS} ${MAGICS_EXTRA_INCLUDE_DIRS} )


add_subdirectory( tools )
add_subdirectory( src )
add_subdirectory( share )
add_subdirectory( apps )
add_subdirectory( test )

if( HAVE_EFAS )
else()
ecbuild_add_resources( TARGET noefas DONT_PACK_DIRS share/magics/efas)
endif()

# Directories not need in the distribution tar ball!
ecbuild_add_resources( TARGET bamboo DONT_PACK_DIRS bamboo)
ecbuild_add_resources( TARGET old_resources
		DONT_PACK_DIRS 
		docs
		toolsjs
		notebook)


############################################################################################
# finalize

ecbuild_pkgconfig(
    NAME         magics
    DESCRIPTION  "Multi-platform meteorological graphics library"
    URL          "https://software.ecmwf.int/magics"
    LIBRARIES    ${MAGICS_LIBRARIES}
)

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()
