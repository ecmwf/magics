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

# TODO:
#  * fix RESOURCES magics.py
#  * unit tests check that certain files are produced and not empty
#  * python unit test
#  * fortran unit test

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
# some variables of this project

option( WITH_QT            "wether to use Qt"        OFF )
option( WITH_CAIRO         "wether to use Cairo"     ON  )
option( MAGICS_ALLHEADERS  "install all headers"     OFF )
option( WITH_GRIB          "enable grib"             ON  )
option( WITH_NETCDF        "enable netcdf"           ON  )
option( WITH_BUFR          "enable bufr"             OFF )
option( WITH_SPOT          "enable spot"             OFF )
option( WITH_ODB           "enable odb"              OFF )
option( WITH_PYTHON        "enable python interface" ON  )
option( WITH_FORTRAN       "enable Fortran interface" ON  )

set( MAGICS_NAME          "Magics" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )


###############################################################################
# projects

# ecbuild_use_package( PROJECT eckit VERSION 0.3 REQUIRED )

if( WITH_ODB)
  ecbuild_use_package( PROJECT odb_api  VERSION 0.9.28 )
endif()

if( WITH_GRIB)
  ecbuild_use_package( PROJECT grib_api VERSION 1.9.16 )
endif()

ecbuild_declare_project()

###############################################################################
# find extra packages

set( CMAKE_THREAD_PREFER_PTHREAD 1 )
find_package( Threads ) 

# set(Boost_USE_STATIC_LIBS        ON)
# set(Boost_NO_SYSTEM_PATHS        ON)
# set(Boost_USE_MULTITHREADED      ON)


find_package( Boost 1.49.0 )

find_package( Proj4  )

ecbuild_find_python( VERSION 2.7 )
if (WITH_PYTHON) 
    find_package( SWIG )
    if( SWIG_FOUND )
        include( ${SWIG_USE_FILE} )
    endif()
endif()

set( NETCDF_CXX 1)
find_package( NetCDF )
if (WITH_NETCDF AND NETCDF_FOUND )
	list( APPEND MAGICS_TPLS  NetCDF )
    set (MAGICS_NETCDF 1)
endif()

find_package( Cairo  )
if( WITH_CAIRO AND Cairo_FOUND )
    	set( MAGICS_CAIRO 1 )
endif()

# find_package(Qt4 4.4.3 REQUIRED QtCore QtGui QtXml )
# if( WITH_QT AND QT_FOUND )
# include( ${QT_USE_FILE} )
#    set( MAGICS_QT 1 )
# endif()

###############################################################################
# contents

set( MAGICS_TPLS            grib_api odb_api NetCDF Cairo Proj4 )
set( MAGICS_INCLUDE_DIRS 
	    ${CMAKE_CURRENT_SOURCE_DIR}/src 
        ${CMAKE_BINARY_SOURCE_DIR}/src 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/common
	    ${Boost_INCLUDE_DIRS} )
set( MAGICS_LIBRARIES    MagPlus )

foreach( _tpl ${MAGICS_TPLS} )
    string( TOUPPER ${_tpl} TPL )
    list( APPEND MAGICS_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS} )
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} )
    list( APPEND MAGICS_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES} )
endforeach()

list( APPEND MAGICS_EXTRA_LIBRARIES     ${CMAKE_THREAD_LIBS_INIT} )

# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS ${ECKIT_DEFINITIONS} )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

include_directories ( ${MAGICS_INCLUDE_DIRS} ${MAGICS_EXTRA_INCLUDE_DIRS} )

# scripts
add_subdirectory( tools )

# source files
add_subdirectory( src )

# shared files
add_subdirectory( share )

# python interface 
add_subdirectory( python )

# Test directory
add_subdirectory( test )

############################################################################################
# finalize

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()

