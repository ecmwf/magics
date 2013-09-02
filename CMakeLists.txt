############################################################################################
# cmake options:
#
#       -DCMAKE_BUILD_TYPE=Debug|RelWithDebInfo|Release|Production
#
#       -DCMAKE_MODULE_PATH=/path/to/ecbuild/cmake
#
#       -DCMAKE_C_COMPILER=gcc
#       -DCMAKE_C_COMPILER=g++
#
#       -DCMAKE_PREFIX_PATH=/path/to/jasper:/path/to/any/package/out/of/place
#       -DBUILD_SHARED_LIBS=OFF

# Dependencies:
#   * cairo
#   * boost
#   * proj4
#   * netcdf
#   * grib_api
#   * odb_api
#   * swig / python
#   ? ghostscript 

cmake_minimum_required( VERSION 2.8.4 FATAL_ERROR )

project( magics CXX )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

include( ecbuild_system )

if( ECBUILD_MACRO_VERSION VERSION_LESS 1.1 )
  message( FATAL_ERROR "${PROJECT_NAME} needs ecbuild >= 1.1" )
endif()

###############################################################################
# projects

# ecbuild_use_package( PROJECT eckit VERSION 0.3 REQUIRED )

ecbuild_use_package( PROJECT odb_api  VERSION 0.9.28 )
ecbuild_use_package( PROJECT grib_api VERSION 1.9.16 )

ecbuild_declare_project()

###############################################################################
# some variables of this project

option( WITH_QT            "wether to use Qt"        OFF )
option( WITH_CAIRO         "wether to use Cairo"     ON  )
option( MAGICS_ALLHEADERS  "install all headers"     OFF )

set( MAGICS_NAME          "Magics" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )

###############################################################################
# find extra packages

ecbuild_find_python( VERSION 2.7 )

find_package( SWIG )
if( SWIG_FOUND )
  include( ${SWIG_USE_FILE} )
endif()

find_package( NetCDF )
find_package( Cairo  )

if( Cairo_FOUND AND WITH_CAIRO )
    set( MAGICS_CAIRO 1 )
endif()

find_package( Proj4  )

# set(Boost_USE_STATIC_LIBS        ON)
set(Boost_NO_SYSTEM_PATHS        ON)
# set(Boost_USE_MULTITHREADED      ON)
find_package( Boost 1.49.0 )

# find_package(Qt4 4.4.3 REQUIRED QtCore QtGui QtXml )
# include( ${QT_USE_FILE} )
# if( QT_FOUND AND WITH_QT )
#    set( MAGICS_QT 1 )
# endif()

###############################################################################
# contents

# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS ${ECKIT_DEFINITIONS} )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

set( MAGICS_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_BINARY_SOURCE_DIR}/src )
set( MAGICS_LIBRARIES    MagPlus )
set( MAGICS_TPLS         grib_api odb_api ) # eckit

# scripts
add_subdirectory( tools )

# source files
add_subdirectory( src )

############################################################################################
# finalize

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()

