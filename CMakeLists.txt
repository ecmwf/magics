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

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

include( ecbuild_system )

if( ECBUILD_MACRO_VERSION VERSION_LESS 1.1 )
  message( FATAL_ERROR "${PROJECT_NAME} needs ecbuild >= 1.1" )
endif()

###############################################################################
# some variables of this project

option( WITH_QT            "wether to use Qt"          OFF )
option( WITH_CAIRO         "wether to use Cairo"       OFF  )
option( WITH_METVIEW  "install all headers"       OFF )
option( WITH_GRIB          "enable grib"               ON  )
option( WITH_NETCDF        "enable netcdf"             ON  )
option( WITH_BUFR          "enable bufr"               OFF )
option( WITH_SPOT          "enable spot"               OFF )
option( WITH_ODB           "enable odb"                OFF )
option( WITH_PYTHON        "enable python interface"   ON  )
option( WITH_FORTRAN       "enable Fortran interface"  ON  )

set( MAGICS_NAME          "Magics" )
set( MAGICS_SITE          "ecmwf" )
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )
set( MAGICS_REFERENCE_VERSION        "2.20.2" )
###############################################################################
# projects

# ecbuild_use_package( PROJECT eckit VERSION 0.3 REQUIRED )

if( WITH_ODB)
  ecbuild_use_package( PROJECT odb_api  VERSION 0.9.28 )
  debug_var( ODB_API_FOUND )
  if( ODB_API_FOUND )
		set ( MAGICS_ODB 1 )
  endif()
endif()
debug_var( MAGICS_ODB )

set( MAGICS_ONLY 1)

if( WITH_METVIEW )
	set( WITH_QT 1 )
    unset(MAGICS_ONLY)
endif()

if( WITH_GRIB )
  ecbuild_use_package( PROJECT grib_api VERSION 1.9.16 )
   if( GRIB_API_FOUND )
        set( MAGICS_GRIB 1 )
  endif()
endif()

if( WITH_BUFR )
  find_package( EMOS  )
   if ( EMOS_FOUND ) 
		set ( MAGICS_BUFR 1 )
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
find_package( Boost 1.49.0 )

find_package( Proj4  )
find_package( EXPAT  )


ecbuild_find_python( VERSION 2.7 )
if( WITH_PYTHON )
    find_package( SWIG )
    if( SWIG_FOUND )
        include(UseSWIG)
    endif()
endif()

if( WITH_NETCDF )
    set( PREFER_NETCDF4 1)
    set( NETCDF_CXX 1 )
    find_package( NetCDF )
    if(NETCDF_FOUND )
		list( APPEND MAGICS_TPLS  NetCDF )
        set(MAGICS_NETCDF 1)
	endif()
endif()

if( WITH_CAIRO )
	find_package( PangoCairo  )
	debug_var(PANGOCAIRO_FOUND)

	if( PANGOCAIRO_FOUND )
    	set( MAGICS_CAIRO 1 )
    endif()
    debug_var ( MAGICS_CAIRO )
endif()


install( FILES ${CMAKE_CURRENT_BINARY_DIR}/magics-config
			DESTINATION ${INSTALL_BIN_DIR} 
			PERMISSIONS OWNER_READ GROUP_READ WORLD_READ 
				        OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)

if( WITH_QT )
	find_package(Qt4 4.4.3 REQUIRED QtCore QtGui QtXml )
	if( QT_FOUND )
		include( ${QT_USE_FILE} )
		set( MAGICS_QT 1)
	endif()
endif()

###############################################################################
# contents

set( MAGICS_TPLS grib_api odb_api EXPAT NetCDF PangoCairo Proj4 EMOS)
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
 \       ${CMAKE_CURRENT_SOURCE_DIR}/src/terralib/utils
	    ${Boost_INCLUDE_DIRS} )
	    
if ( MAGICS_ODB ) 
	list ( APPEND MAGICS_INCLUDE_DIRS 
			${CMAKE_CURRENT_SOURCE_DIR}/src/oda 
			${ODB_API_INCLUDE_DIRS}/../eclib 
		)
endif()
	    
	    
debug_var (${PANGOCAIRO_LIBRARIES})

set( MAGICS_LIBRARIES    MagPlus )

foreach( _tpl ${MAGICS_TPLS} )
    string( TOUPPER ${_tpl} TPL )
    list( APPEND MAGICS_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  )
    list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} )
    list( APPEND MAGICS_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES} )
endforeach()
message(STATUS "MAGICS_EXTRA_DEFINITIONS    => ${MAGICS_EXTRA_LIBRARIES}")
message(STATUS "PANGOCAIRO_LIBRARIES  => ${PANGOCAIRO_LIBRARIES}")
message(STATUS "MAGICS_EXTRA_INCLUDE_DIRS => ${MAGICS_EXTRA_INCLUDE_DIRS}")
message(STATUS "MAGICS_EXTRA_LIBRARIES    => ${MAGICS_EXTRA_LIBRARIES}")

if ( MAGICS_BUFR )
	list( APPEND MAGICS_EXTRA_DEFINITIONS   fortfloat=double fortint=int boolean=bool )
endif()

if ( MAGICS_QT )
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${QT_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES     ${QT_LIBRARIES} )
endif()

list( APPEND MAGICS_EXTRA_LIBRARIES     ${CMAKE_THREAD_LIBS_INIT} )

# set_directory_properties( PROPERTIES COMPILE_DEFINITIONS ${ECKIT_DEFINITIONS} )
get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

include_directories( ${MAGICS_INCLUDE_DIRS} ${MAGICS_EXTRA_INCLUDE_DIRS} )

set(  __magics_inc_tmp ${MAGICS_EXTRA_INCLUDE_DIRS} )
set(MAGICS_EXTRA_INCLUDES "")

foreach( inc ${__magics_inc_tmp} ) 
  set(MAGICS_EXTRA_INCLUDES "${MAGICS_EXTRA_INCLUDES} -I${inc}" )
endforeach()
debug_var(MAGICS_EXTRA_INCLUDES)

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
add_subdirectory( regression )

# Directories not need in the distribution tar ball!
ecbuild_add_resources( TARGET src_MvObs DONT_PACK_DIRS src/MvObs)
ecbuild_add_resources( TARGET src_libTable DONT_PACK_DIRS src/libTable)
ecbuild_add_resources( TARGET docs DONT_PACK_DIRS docs)
ecbuild_add_resources( TARGET regression 
		DONT_PACK_DIRS tools/versioncmp 
					tools/regression 
					tools/xml 
					tools/use_scripts
					tools/versioncmp_folders)




set(  __magics_tmp ${MAGICS_EXTRA_LIBRARIES} )

list(REMOVE_ITEM __magics_tmp debug )
list(REMOVE_ITEM __magics_tmp optimized )

set(MAGICS_EXTRA_LIBS "")
debug_var(__magics_tmp)

foreach( lib ${__magics_tmp} )
  if( lib MATCHES "^/.*" )
# Need to split -L/path and -l lib
	 get_filename_component(path lib PATH)
	 #set(MAGICS_EXTRA_LIBS "${MAGICS_EXTRA_LIBS} ${lib}" )
  else()
    if( NOT lib MATCHES "^-l.*" )
		set( lib "-l${lib}" )
	endif()
  endif()
  
  set(MAGICS_EXTRA_LIBS "${MAGICS_EXTRA_LIBS} ${lib}" )
endforeach()




configure_file( magics-config.in  magics-config @ONLY )					
# ecbuild/cmake is needed in the the distribution tar ball!

					
############################################################################################
# finalize

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()


