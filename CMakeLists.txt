############################################################################################
# cmake options:
#
#       -DCMAKE_BUILD_TYPE=Debug|RelWithDebInfo|Release|Production
#       -DCMAKE_INSTALL_PREFIX=/path/to/install
#
#       -DCMAKE_MODULE_PATH=/path/to/ecbuild/cmake
#
#       -DCMAKE_C_COMPILER=gcc
#       -DCMAKE_C_COMPILER=g++
#
#       -DCMAKE_PREFIX_PATH=/path/to/jasper:/path/to/any/package/out/of/place
#       -DBUILD_SHARED_LIBS=OFF

cmake_minimum_required( VERSION 2.6.4 FATAL_ERROR )

project( magics CXX )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

include( ecbuild_system )

###############################################################################
# projects

ecbuild_use_package( PROJECT eclib    NAME EcLib    REQUIRED )
# ecbuild_use_package( PROJECT odb_api  NAME ODB_API  )
ecbuild_use_package( PROJECT grib_api NAME GRIB_API )

ecbuild_declare_project()

###############################################################################
# some variables of this project

option( WITH_QT            "wether to use Qt"     OFF  )
option( WITH_CAIRO         "wether to use Cairo"     ON  )
option( MAGICS_ALLHEADERS  "install all teh headers" OFF )

set( MAGICS_NAME          "Magics" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )

###############################################################################
# find extra packages

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

if( QT_FOUND AND WITH_QT )
    set( MAGICS_QT 1 )
endif()

###############################################################################
# contents

# scripts
add_subdirectory( tools )

# source files
add_subdirectory( src )

############################################################################################
# finalize

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()

