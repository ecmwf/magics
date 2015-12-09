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
#  * fix python module generation Magics.py to __init__.py
#  * cpack ecbuild/cmake macros into tar.gz
#  * unit tests check that certain files are produced and not empty
#  * pkg-config needed for metview

# Missing dependencies:
#   * ghostscript ( is it really needed? -- to confirm )

cmake_minimum_required( VERSION 2.8.4 FATAL_ERROR )

project( magics CXX )

# make sure that the header files are installed into include/magics
# note that this needs to be done before ecbuild_declare_project()
# to ensure that the ecbuild header files are also put there
# note also that we need to CACHE this so that ecbuild_declare_project() does not overwrite it
set(MAGICS_INSTALL_INCLUDE_DIR include/magics CACHE PATH "Magics installation directory for header files")
set(INSTALL_INCLUDE_DIR include/magics CACHE PATH "Magics installation directory for header files")

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 1.9 )

###############################################################################
# some variables of this project

ecbuild_use_package(PROJECT grib_api VERSION 1.12.3 REQUIRED)
# grib_api is required for now
set( HAVE_GRIB 1 )

ecbuild_add_option( FEATURE BUFR
                    DEFAULT ON
                    DESCRIPTION "BUFR support"
                    REQUIRED_PACKAGES "PROJECT libemos VERSION 4.0.5" )

ecbuild_add_option( FEATURE ODB
                    DEFAULT ON
                    DESCRIPTION "ODB support"
                    REQUIRED_PACKAGES 
						"PROJECT odb_api VERSION 0.10.2"
						"PROJECT eckit   VERSION 0.9.0" )

ecbuild_add_option( FEATURE CAIRO
                    DEFAULT ON
                    DESCRIPTION "cairo support[png/jpeg]"
                    REQUIRED_PACKAGES PangoCairo )

ecbuild_add_option( FEATURE GEOTIFF
                    DEFAULT OFF
                    DESCRIPTION "geotiff support [implies cairo]"
                    CONDITION HAVE_CAIRO
                    REQUIRED_PACKAGES GeoTIFF )

ecbuild_add_option( FEATURE NETCDF
                    DEFAULT ON
                    DESCRIPTION "enable netcdf support"
                    REQUIRED_PACKAGES "NetCDF 4 COMPONENTS C CXX" )

ecbuild_add_option( FEATURE SPOT
                    DEFAULT OFF
                    DESCRIPTION "enable spot support"
                    REQUIRED_PACKAGES "PROJECT spot" )

ecbuild_add_option( FEATURE PYTHON
                    DEFAULT ON
                    DESCRIPTION "enable python interface"
                    REQUIRED_PACKAGES Python SWIG NumPy )

if( HAVE_PYTHON )
   include(UseSWIG)
endif()

ecbuild_add_option( FEATURE FORTRAN
                    DEFAULT ON
                    DESCRIPTION "enable Fortran interface" )

if( HAVE_FORTRAN )
	ecbuild_enable_fortran( REQUIRED )
endif()

ecbuild_add_option( FEATURE METVIEW
                    DEFAULT OFF
                    DESCRIPTION "enable Metview interface" )

ecbuild_add_option( FEATURE METVIEW_NO_QT
                    DEFAULT OFF
                    DESCRIPTION "enable Metview interface without Qt" )

ecbuild_add_option( FEATURE QT5
                    DEFAULT OFF
                    DESCRIPTION "use Qt5 instead of version 4" )

ecbuild_add_option( FEATURE PNG
                    DESCRIPTION "support for PNG decoding/encoding"
                    DEFAULT ON
                    REQUIRED_PACKAGES PNG
)

if( HAVE_PNG )
    add_definitions( ${PNG_DEFINITIONS} )
endif()

option( BUILD_SHARED_LIBS  "Set to BOTH to build static library in addition to shared"  ON )
option( ENABLE_REGRESSION        "Internal use only: enable regression test"  OFF  )
option( ENABLE_REGRESSION_UPLOAD "Internal use only : enable upload of reference image"  OFF  )

set( MAGICS_NAME          "Magics" )
set( MAGICS_EXCEPTION     "ON" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )

# Regression definitions
set( MAGICS_REFERENCE_VERSIONS            "2.26.0" )
set( MAGICS_HTML_ROOT        "${CMAKE_BINARY_DIR}/regression/html")
file(MAKE_DIRECTORY ${MAGICS_HTML_ROOT} )

set( MAG_PYTHON_PATH ${CMAKE_BINARY_DIR}/python )

###############################################################################
# projects

# ecbuild_use_package( PROJECT eckit VERSION 0.3 REQUIRED )

#cmake_add_cxx_flags("-gdwarf-2")
cmake_add_c_flags("-std=c99")

### Metview and Qt

if(HAVE_METVIEW AND HAVE_METVIEW_NO_QT)
    message(FATAL_ERROR "Do not set both HAVE_METVIEW and HAVE_METVIEW_NO_QT - only set one. You may have to remove your CMakeCache.txt to clear these settings.")
endif()

set( qt no )
set( metview no )

if( HAVE_METVIEW_NO_QT )
	set( metview yes )
	unset(MAGICS_ONLY)
endif()

if( HAVE_METVIEW )
    set ( metview yes)
    message(STATUS "TESTING for Metview ...")
    unset(MAGICS_ONLY)

    if( HAVE_QT5 )
      find_package(Qt5Widgets REQUIRED)
      if( Qt5Widgets_FOUND )
          message(STATUS "Qt5 was found ... ${Qt5Widgets_VERSION_STRING}")
          include_directories(${Qt5Widgets_INCLUDE_DIRS})
          set( MAGICS_QT 1)
          set( MAGICS_QT5 1)
          set( qt yes)
          add_definitions( -DMAGICS_QT5 )
      else()
          message(FATAL_ERROR "Qt5 was NOT found ...")
      endif()
    else()
    find_package(Qt4 4.4.3 REQUIRED QtCore QtGui QtXml )
    if( QT_FOUND )
		  include( ${QT_USE_FILE} )
		  set( MAGICS_QT 1)
		  set( qt yes)
    endif()
    endif()
endif()

ecbuild_declare_project()

###############################################################################
# find extra packages

set( CMAKE_THREAD_PREFER_PTHREAD 1 )
find_package( Threads ) 

# set(Boost_USE_STATIC_LIBS        ON)
# set(Boost_NO_SYSTEM_PATHS        ON)
# set(Boost_USE_MULTITHREADED      ON)
ecbuild_add_extra_search_paths( boost )

find_package( Boost 1.49.0 REQUIRED)
find_package( Proj4 REQUIRED )
find_package( EXPAT  REQUIRED )

###############################################################################
# contents

set( MAGICS_TPLS grib_api spot EXPAT NetCDF PangoCairo PNG Proj4 libemos )

set( MAGICS_INCLUDE_DIRS 
     ${CMAKE_CURRENT_SOURCE_DIR}/src 
     ${CMAKE_BINARY_SOURCE_DIR}/src 
     ${CMAKE_CURRENT_SOURCE_DIR}/src/common 
     ${CMAKE_CURRENT_BINARY_DIR}/src/params 
     ${CMAKE_CURRENT_BINARY_DIR}/src 
     ${CMAKE_CURRENT_SOURCE_DIR}/src/common  
     ${CMAKE_CURRENT_SOURCE_DIR}/src/basic
     ${CMAKE_CURRENT_SOURCE_DIR}/src/web
     ${CMAKE_CURRENT_SOURCE_DIR}/src/visualisers
     ${CMAKE_CURRENT_SOURCE_DIR}/src/drivers
     ${CMAKE_CURRENT_SOURCE_DIR}/src/drivers/MgQ
     ${CMAKE_CURRENT_SOURCE_DIR}/src/eckit_readers
     ${CMAKE_CURRENT_SOURCE_DIR}/src/decoders
     ${CMAKE_CURRENT_SOURCE_DIR}/src/terralib
     ${CMAKE_CURRENT_SOURCE_DIR}/src/terralib/kernel
     ${CMAKE_CURRENT_SOURCE_DIR}/src/terralib/utils
     ${CMAKE_CURRENT_SOURCE_DIR}/src/libMagWrapper
     ${Boost_INCLUDE_DIRS}
     ${PANGOCAIRO_INCLUDE_DIRS} )

if( HAVE_ODB )
  list( APPEND MAGICS_TPLS odb_api )
  list( APPEND MAGICS_INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/src/oda
        ${ODB_API_INCLUDE_DIRS}
		${ECKIT_INCLUDE_DIRS} )
endif()

set( MAGICS_LIBRARIES    MagPlus )

foreach( _tpl ${MAGICS_TPLS} )
    string( TOUPPER ${_tpl} TPL )
    if( NOT ${TPL} STREQUAL "LIBEMOS" ) # don't add libemos definitions
    list( APPEND MAGICS_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  )
    endif()
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} )
    list( APPEND MAGICS_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES} )
endforeach()

if( HAVE_BUFR )
	list( APPEND MAGICS_EXTRA_DEFINITIONS   fortfloat=double fortint=int )
    ecbuild_find_fortranlibs()
	list( APPEND MAGICS_EXTRA_LIBRARIES ${FORTRAN_LIBRARIES} )
endif()
if( WITH_QT_DEBUG )
	list( APPEND MAGICS_EXTRA_DEFINITIONS QT_NO_DEBUG_OUTPUT )
endif()

if( MAGICS_QT )
  if( MAGICS_QT5 )
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${Qt5Widgets_INCLUDE_DIR} )
    list( APPEND MAGICS_EXTRA_LIBRARIES     ${Qt5Widgets_LIBRARIES} )
  else()
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${QT_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES     ${QT_LIBRARIES} )
endif()
endif()

if( HAVE_GEOTIFF )
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${GEOTIFF_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES  ${GEOTIFF_LIBRARY} )
  list( APPEND MAGICS_EXTRA_DEFINITIONS HAVE_GEOTIFF )
endif()

list( APPEND MAGICS_EXTRA_LIBRARIES     ${CMAKE_THREAD_LIBS_INIT} )

ecbuild_debug("MAGICS_EXTRA_DEFINITIONS  => ${MAGICS_EXTRA_DEFINITIONS}")
ecbuild_debug("MAGICS_EXTRA_INCLUDE_DIRS => ${MAGICS_EXTRA_INCLUDE_DIRS}")
ecbuild_debug("MAGICS_EXTRA_LIBRARIES    => ${MAGICS_EXTRA_LIBRARIES}")

# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS "${ECKIT_DEFINITIONS}" )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

include_directories( ${MAGICS_INCLUDE_DIRS} ${MAGICS_EXTRA_INCLUDE_DIRS} )

# scripts
add_subdirectory( tools )

# source files
add_subdirectory( src )

# shared files
add_subdirectory( share )

# python interface 
add_subdirectory( python )

# apps directory
add_subdirectory( apps )

# Test directory
add_subdirectory( test )
if( ENABLE_REGRESSION )
    add_subdirectory( regression )
endif()

# Directories not need in the distribution tar ball!
ecbuild_add_resources( TARGET old_src DONT_PACK_DIRS src/MvObs src/libTable src/xml src/terralib/functions)
ecbuild_add_resources( TARGET bamboo DONT_PACK_DIRS bamboo)
ecbuild_add_resources( TARGET old_ressources 
		DONT_PACK_DIRS tools/versioncmp 
		tools/regression 
		regression
		docs
		toolsjs
		test/old
		tools/xml 
		tools/use_scripts
		tools/versioncmp_folders)

ecbuild_add_resources( TARGET internal 
		DONT_PACK configure.lxab
		configure.local
		configure.ecgb
		configure.ecmwf
)

############################################################################################
# finalize

set( MAGICS_DESCRIPTION "Multi-platform meteorological graphics library" )
set( MAGICS_URL "https://software.ecmwf.int/wiki/display/MAGP/Magics" )
ecbuild_pkgconfig()

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()
