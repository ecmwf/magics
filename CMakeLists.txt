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

ecbuild_requires_macro_version( 1.8 )

###############################################################################
# some variables of this project

ecbuild_use_package(PROJECT grib_api VERSION 1.12.3 REQUIRED)

ecbuild_add_option( FEATURE BUFR
                    DEFAULT ON
                    DESCRIPTION "BUFR support"
                    REQUIRED_PACKAGES "PROJECT libemos VERSION 4.0.5" )

ecbuild_add_option( FEATURE ODB
                    DEFAULT ON
                    DESCRIPTION "ODB support"
                    REQUIRED_PACKAGES "PROJECT odb_api VERSION 0.10.2" )

ecbuild_add_option( FEATURE CAIRO
                    DEFAULT ON
                    DESCRIPTION "cairo support[png/jpeg]"
                    REQUIRED_PACKAGES PangoCairo )

ecbuild_add_option( FEATURE GEOTIFF
                    DEFAULT OFF
                    DESCRIPTION "geotiff support [implies cairo]"
                    CONDITION HAVE_CAIRO
                    REQUIRED_PACKAGES GeoTIFF )

option( ENABLE_NETCDF         "enable netcdf support"             ON  )

option( ENABLE_SPOT           "enable spot support"               OFF )

option( ENABLE_PYTHON         "enable python interface"   ON  )
option( ENABLE_FORTRAN        "enable fortran interface"  ON  )

option( ENABLE_METVIEW        "enable Metview interface"  OFF  )
option( ENABLE_METVIEW_NO_QT  "enable Metview interface without Qt"    OFF )
option( ENABLE_QT5            "use Qt5 instead of version 4"        OFF )

option( ENABLE_STATIC_LIBRARY    "Internal Build static Library"    OFF  )
option( ENABLE_REGRESSION        "Internal use only: enable regression test"  OFF  )
option( ENABLE_REGRESSION_UPLOAD "Internal use only : enable upload of reference image"  OFF  )

set( MAGICS_NAME          "Magics" )
set( MAGICS_EXCEPTION     "ON" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )

# Regression definitions
set( MAGICS_REFERENCE_VERSIONS            "2.24.7" )
set( MAGICS_HTML_ROOT        "${CMAKE_BINARY_DIR}/regression/html")
file(MAKE_DIRECTORY ${MAGICS_HTML_ROOT} )

set( MAG_PYTHON_PATH ${CMAKE_BINARY_DIR}/python )

###############################################################################
# projects

# ecbuild_use_package( PROJECT eckit VERSION 0.3 REQUIRED )

### fortran

if( ENABLE_FORTRAN )
	ecbuild_enable_fortran( REQUIRED )
endif()

### python

if( ENABLE_PYTHON )
	ecbuild_find_python( REQUIRED )

    debug_var( PYTHON_INCLUDE_DIRS )
    debug_var( PYTHON_LIBRARIES )

    if( NOT PYTHON_INCLUDE_DIRS OR NOT PYTHON_LIBRARIES )
        message( FATAL_ERROR "Couldn't find Python libraries" )
    endif()

	find_package( SWIG REQUIRED )
	find_package( NumPy REQUIRED )
endif()
if( SWIG_FOUND )
   include(UseSWIG)
endif()

### grib

#cmake_add_cxx_flags("-gdwarf-2")
cmake_add_c_flags("-std=c99")

#set( grib no )
#if( HAVE_GRIB )
   set( MAGICS_GRIB 1 )
   set( grib yes )
#endif()

### netcdf

set( netcdf no )
# set( PREFER_NETCDF4 1)
set( NETCDF_CXX 1 )
if ( ENABLE_NETCDF )
  find_package( NetCDF 4 COMPONENTS C CXX REQUIRED)
endif()
if ( NETCDF_FOUND )
    list( APPEND MAGICS_TPLS  NetCDF )
    set(MAGICS_NETCDF 1)
	set( netcdf yes)
endif()

### odb

if( HAVE_ODB )

    get_filename_component( odb_api_install_dir ${ODB_API_CMAKE_DIR}/../../../lib ABSOLUTE )
    get_filename_component( eckit_install_dir   ${ECKIT_CMAKE_DIR}/../../../lib ABSOLUTE )

      set( MAGICS_ODB 1 )

endif()

### spot

if( ENABLE_SPOT )
    ecbuild_use_package( PROJECT spot  REQUIRED)
    if( SPOT_FOUND )
        set (MAGICS_SPOT 1)
    endif()
else() 
endif()

### bufr

if( HAVE_BUFR ) # set by ecbuild_add_option()
   set( MAGICS_BUFR 1 )
endif()


### cairo

if( HAVE_CAIRO )
	set( MAGICS_CAIRO 1 )
  if( HAVE_GEOTIFF )
	      set( MAGICS_GEOTIFF 1 )
	   endif()
	endif()

ecbuild_add_option( FEATURE PNG
    DESCRIPTION "support for PNG decoding/encoding"
    DEFAULT ON
    REQUIRED_PACKAGES PNG
)

if( HAVE_PNG )
    add_definitions( ${PNG_DEFINITIONS} )
endif()

### Metview and Qt

if(ENABLE_METVIEW AND ENABLE_METVIEW_NO_QT)
    message(FATAL_ERROR "Do not set both ENABLE_METVIEW and ENABLE_METVIEW_NO_QT - only set one. You may have to remove your CMakeCache.txt to clear these settings.")
endif()

set( qt no )
set( metview no )

if( ENABLE_METVIEW_NO_QT )
	set( metview yes )
	unset(MAGICS_ONLY)
endif()

if( ENABLE_METVIEW )
    set ( metview yes)
    message(STATUS "TESTING for Metview ...")
    unset(MAGICS_ONLY)

    if( ENABLE_QT5 )
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
    else( ENABLE_QT5 )
    find_package(Qt4 4.4.3 REQUIRED QtCore QtGui QtXml )
    if( QT_FOUND )
		  include( ${QT_USE_FILE} )
		  set( MAGICS_QT 1)
		  set( qt yes)
    endif()
    endif( ENABLE_QT5 )
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


install( FILES ${CMAKE_CURRENT_BINARY_DIR}/magics-config
			DESTINATION ${INSTALL_BIN_DIR} 
			PERMISSIONS OWNER_READ GROUP_READ WORLD_READ 
				        OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)


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
        ${Boost_INCLUDE_DIRS} )
    
if( MAGICS_ODB )
    list( APPEND MAGICS_INCLUDE_DIRS
		${CMAKE_CURRENT_SOURCE_DIR}/src/oda 
		${ODB_API_INCLUDE_DIRS}/../eclib 
	)
endif()


if (metview)
    # required if building as part of a bundle
    list( APPEND MAGICS_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src/libMagWrapper)
endif()


set( MAGICS_LIBRARIES    MagPlus )



# remove eckit_cmd because it's not needed for Magics, and causes problems when linked
# with Metview
list(REMOVE_ITEM ODB_API_LIBRARIES eckit_cmd)


foreach( _tpl ${MAGICS_TPLS} )
    string( TOUPPER ${_tpl} TPL )
    if( NOT ${TPL} STREQUAL "LIBEMOS" ) # don't add libemos definitions
    list( APPEND MAGICS_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  )
    endif()
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} )
    list( APPEND MAGICS_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES} )
endforeach()

if( MAGICS_ODB )
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${ODB_API_INCLUDE_DIRS} )
    list( APPEND MAGICS_EXTRA_LIBRARIES     Odb )
endif()

if( MAGICS_BUFR )
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

if( MAGICS_GEOTIFF )
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${GEOTIFF_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES  ${GEOTIFF_LIBRARY} )
	list( APPEND MAGICS_EXTRA_DEFINITIONS MAGICS_GEOTIFF )
endif()

list( APPEND MAGICS_EXTRA_LIBRARIES     ${CMAKE_THREAD_LIBS_INIT} )

message(STATUS "MAGICS_EXTRA_DEFINITIONS  => ${MAGICS_EXTRA_DEFINITIONS}")
message(STATUS "MAGICS_EXTRA_INCLUDE_DIRS => ${MAGICS_EXTRA_INCLUDE_DIRS}")
message(STATUS "MAGICS_EXTRA_LIBRARIES    => ${MAGICS_EXTRA_LIBRARIES}")



# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS "${ECKIT_DEFINITIONS}" )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

include_directories( ${MAGICS_INCLUDE_DIRS} ${MAGICS_EXTRA_INCLUDE_DIRS} )

set(  __magics_inc_tmp ${MAGICS_EXTRA_INCLUDE_DIRS} )
set(MAGICS_EXTRA_INCLUDES "")

foreach( inc ${__magics_inc_tmp} ) 
  set(MAGICS_EXTRA_INCLUDES "${MAGICS_EXTRA_INCLUDES} -I${inc}" )
endforeach()

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


set(  __magics_tmp ${MAGICS_EXTRA_LIBRARIES} )

list(REMOVE_ITEM __magics_tmp debug )
list(REMOVE_ITEM __magics_tmp optimized )
if( ODB_API_FOUND )
    list(REMOVE_ITEM __magics_tmp Odb)
    list(REMOVE_ITEM __magics_tmp eckit)
endif()

set(MAGICS_EXTRA_LIBS "")

foreach( lib ${__magics_tmp} )
  if( lib MATCHES "^/.*" )
# Need to split -L/path and -l libd
	
	 get_filename_component(ext ${lib} EXT)
	  if ( ${ext} MATCHES ${CMAKE_SHARED_LIBRARY_SUFFIX} )
		
		get_filename_component(path ${lib} PATH)
		
	  endif()
	 
  else()
    if( NOT lib MATCHES "^-l.*" )
		set( lib "-l${lib}" )
	endif()
  endif()
  
  set(MAGICS_EXTRA_LIBS "${MAGICS_EXTRA_LIBS} ${lib}" )
endforeach()


if( ODB_API_FOUND )
    set(MAGICS_EXTRA_LIBS "${MAGICS_EXTRA_LIBS} -L${odb_api_install_dir} -lOdb")
    set(MAGICS_EXTRA_LIBS "${MAGICS_EXTRA_LIBS} -L${eckit_install_dir} -leckit")
    set(MAGICS_EXTRA_LIBS "${MAGICS_EXTRA_LIBS} -L${eckit_install_dir} -leckit_ecml")
endif()

configure_file( magics-config.in  magics-config @ONLY )					
# ecbuild/cmake is needed in the the distribution tar ball!
					
############################################################################################
# finalize

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()
