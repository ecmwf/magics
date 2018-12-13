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

cmake_minimum_required( VERSION 3.1.0 FATAL_ERROR )

project( magics CXX )

# make sure that the header files are installed into include/magics
# note that this needs to be done before ecbuild_declare_project()
# to ensure that the ecbuild header files are also put there
# note also that we need to CACHE this so that ecbuild_declare_project() does not overwrite it
set(INSTALL_INCLUDE_DIR include/magics CACHE PATH "Magics installation directory for header files")

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include(ecbuild_system NO_POLICY_SCOPE)

ecbuild_requires_macro_version( 2.9 )

set (CMAKE_CXX_STANDARD 14)
set (CMAKE_C_STANDARD 99)

###############################################################################
# some variables of this project

ecbuild_use_package( PROJECT eccodes VERSION 2.9.2 REQUIRED
                     FAILURE_MSG "ecCodes is required in order to build Magics. Note that GRIB_API is no longer supported in Magics" )

set( grib_handling_pkg eccodes )
set( HAVE_GRIB 1 )
set( HAVE_BUFR 1 )


ecbuild_add_option( FEATURE ODB
                    DEFAULT OFF
                    DESCRIPTION "ODB support"
                    REQUIRED_PACKAGES "PROJECT odb_api VERSION 0.10.2")
					#	"PROJECT eckit   VERSION 0.9.0" )

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
                    DESCRIPTION "geotiff support [implies cairo]"
                    CONDITION HAVE_CAIRO
                    REQUIRED_PACKAGES GeoTIFF )

ecbuild_add_option( FEATURE NETCDF
                    DEFAULT ON
                    DESCRIPTION "enable netcdf support"
                    REQUIRED_PACKAGES "NetCDF 4 COMPONENTS C" )

ecbuild_add_option( FEATURE SPOT
                    DEFAULT OFF
                    DESCRIPTION "enable spot support"
                    REQUIRED_PACKAGES "PROJECT spot" )

find_package(PythonInterp REQUIRED)
find_package(Jinja2 REQUIRED)

ecbuild_add_option( FEATURE PYTHON
                    DEFAULT ON
                    DESCRIPTION "enable python interface"
                    REQUIRED_PACKAGES "Python NO_LIBS" NumPy )

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

option( BUILD_SHARED_LIBS  "Set to BOTH to build static library in addition to shared"  ON )
option( ENABLE_REGRESSION        "Internal use only: enable regression test"  OFF  )
option( ENABLE_REGRESSION_UPLOAD "Internal use only : enable upload of reference image"  OFF  )

set( MAGICS_NAME          "Magics" )
set( MAGICS_EXCEPTION     "ON" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )

set( MAGICS_REFERENCE_VERSIONS            "3.3.1" )
set( MAGICS_HTML_ROOT        "${CMAKE_BINARY_DIR}/regression/html")
file(MAKE_DIRECTORY ${MAGICS_HTML_ROOT} )

set( MAG_PYTHON_PATH ${CMAKE_CURRENT_BINARY_DIR}/python )

###############################################################################
# projects

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

find_package( Boost 1.53.0 REQUIRED)
find_package( Proj4 REQUIRED )
find_package( EXPAT REQUIRED )

###############################################################################
# contents

set( MAGICS_TPLS eccodes spot EXPAT NetCDF Proj4 )

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
     ${CMAKE_CURRENT_SOURCE_DIR}/src/libMagWrapper
     ${Boost_INCLUDE_DIRS} )

if( HAVE_CAIRO )
  list( APPEND MAGICS_TPLS PangoCairo)
  list( APPEND MAGICS_INCLUDE_DIRS ${PANGOCAIRO_INCLUDE_DIRS})
endif()

if( HAVE_ODB )
  list( APPEND MAGICS_TPLS odb_api )
  list( APPEND MAGICS_INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/src/oda
        ${ODB_API_INCLUDE_DIRS}
		${ECKIT_INCLUDE_DIRS} )
endif()

set( MAGICS_LIBRARIES MagPlus )

foreach( _tpl ${MAGICS_TPLS} )
    string( TOUPPER ${_tpl} TPL )
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

ecbuild_debug("MAGICS_EXTRA_DEFINITIONS  => ${MAGICS_EXTRA_DEFINITIONS}")
ecbuild_debug("MAGICS_EXTRA_INCLUDE_DIRS => ${MAGICS_EXTRA_INCLUDE_DIRS}")
ecbuild_debug("MAGICS_EXTRA_LIBRARIES    => ${MAGICS_EXTRA_LIBRARIES}")

# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS "${ECKIT_DEFINITIONS}" )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

include_directories( ${MAGICS_INCLUDE_DIRS} ${MAGICS_EXTRA_INCLUDE_DIRS} )


add_subdirectory( tools )
add_subdirectory( src )
add_subdirectory( share )
add_subdirectory( python )
add_subdirectory( apps )
add_subdirectory( test )
if( ENABLE_REGRESSION )
    add_subdirectory( regression )
endif()

# Directories not need in the distribution tar ball!
ecbuild_add_resources( TARGET old_src DONT_PACK_DIRS src/MvObs src/libTable src/xml src/terralib/functions)
ecbuild_add_resources( TARGET bamboo DONT_PACK_DIRS bamboo)
ecbuild_add_resources( TARGET old_resources
		DONT_PACK_DIRS tools/versioncmp
		tools/regression
		regression
		docs
		toolsjs
		notebook
		test/old
		tools/xml
		tools/use_scripts
		tools/versioncmp_folders)

ecbuild_add_resources( TARGET notebook
    DONT_PACK_DIRS notebook )

ecbuild_add_resources( TARGET internal
		DONT_PACK configure.lxab
		configure.local
		configure.ecgb
		configure.ecmwf
)

############################################################################################
# finalize

set( MAGICS_DESCRIPTION "Multi-platform meteorological graphics library" )
set( MAGICS_URL "https://software.ecmwf.int/magics" )

ecbuild_pkgconfig()

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()
