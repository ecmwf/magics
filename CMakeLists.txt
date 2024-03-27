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

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")
find_package( ecbuild 3.4 REQUIRED )

project( magics VERSION 4.15.3 LANGUAGES CXX )

# make sure that the header files are installed into include/magics
# note that this needs to be done before ecbuild_declare_project()
# to ensure that the ecbuild header files are also put there
# note also that we need to CACHE this so that ecbuild_declare_project() does not overwrite it
set(INSTALL_INCLUDE_DIR include/magics CACHE PATH "Magics installation directory for header files")

include(ecbuild_system NO_POLICY_SCOPE)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 99)

###############################################################################
# some build options for this project


ecbuild_add_option( FEATURE CAIRO
                    DEFAULT ON
                    DESCRIPTION "cairo support[png/jpeg]"
                    REQUIRED_PACKAGES PangoCairo)

ecbuild_add_option( FEATURE DOCUMENTATION
                    DEFAULT OFF
                    DESCRIPTION "Adding support for automatic documentation"
                  )


string(STRIP "${PANGOCAIRO_LIBRARIES}" PANGOCAIRO_LIBRARIES)
if(PANGOCAIRO_FOUND)
 set(HAVE_CAIRO ON)
  ecbuild_enable_feature( CAIRO )
endif()

ecbuild_add_option( FEATURE GEOTIFF
                    DEFAULT OFF
                    DESCRIPTION "geotiff support [implies cairo] -  work on progress"
                    CONDITION HAVE_CAIRO
                    REQUIRED_PACKAGES GeoTIFF )

ecbuild_add_option( FEATURE WEBP
                    DEFAULT OFF
                    DESCRIPTION "webp support [implies cairo]"
                    CONDITION HAVE_CAIRO
                    REQUIRED_PACKAGES WEBP )

ecbuild_add_option( FEATURE NETCDF
                    DEFAULT ON
                    DESCRIPTION "enable netcdf support"
                    REQUIRED_PACKAGES "NetCDF 4 COMPONENTS C" )


ecbuild_add_option( FEATURE ODB
                    DEFAULT OFF
                    DESCRIPTION "ODB support - to be deprecated"
                    REQUIRED_PACKAGES "odc" )

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
set( MAGICS_INSTALL_PATH  ${CMAKE_INSTALL_PREFIX} )


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
    ecbuild_info("TESTING for Qt6 ...")
    unset(MAGICS_ONLY)

    find_package(Qt6Widgets)
    if( Qt6Widgets_FOUND )
        ecbuild_info("Qt6 was found ... ${Qt6Widgets_VERSION_STRING}")
        include_directories(${Qt6Widgets_INCLUDE_DIRS})
        set( MAGICS_QT 1)
        set( MAGICS_QT6 1)
        set( qt yes)
    else()
        find_package(Qt5Widgets REQUIRED)
        if( Qt5Widgets_FOUND )
            ecbuild_info("Qt5 was found ... ${Qt5Widgets_VERSION_STRING}")
            include_directories(${Qt5Widgets_INCLUDE_DIRS})
            set( MAGICS_QT 1)
            set( MAGICS_QT5 1)
            set( qt yes)
        else()
            ecbuild_critical("Neither Qt5 nor Qt6 were found ...")
        endif()
    endif()
endif()

ecbuild_declare_project()

###############################################################################
# find extra packages

ecbuild_find_package( NAME eccodes VERSION 2.24.0 REQUIRED
                      FAILURE_MSG "ecCodes is required in order to build Magics" )
set( HAVE_GRIB 1 )
set( HAVE_BUFR 1 )




# adding NO_MODULE means that if we don't find it, CMake will not generate
# strange error messages about not being able to find the FindProj4.cmake file.
# this should find proj if it has been installed via CMake, but if it has been
# installed via autotools, then we will need to try another method
#ecbuild_find_package(NAME PROJ4 VERSION 6.1.0 QUIET )

# find_package( PROJ4 6.1.0 NO_MODULE )
# if(NOT PROJ4_FOUND)
find_package(PROJ 6.1.0 REQUIRED)
if (NOT PROJ_FOUND)
    message(FATAL_ERROR "Package 'proj' was not found - at least version 6.1.0 required.")
endif()

message("PROJ include: ${PROJ_INCLUDE_DIR}")
message("PROJ library: ${PROJ_LIBRARIES}")
list( APPEND MAGICS_PRIVATE_INCLUDE_DIRS ${PROJ_INCLUDE_DIR})
# endif()


find_package( EXPAT REQUIRED )   # for MagML

if(APPLE)
  find_package( zlib REQUIRED )  # dependency of minizip - KMZ support
endif(APPLE)

### check support for DL library
#
#  code taken from eckit (src/eckit/CMakeList.txt)
#
if( NOT EC_OS_NAME MATCHES "windows" )
check_include_files( dlfcn.h magics_HAVE_DLFCN_H )

if(NOT magics_HAVE_DLFCN_H)
message(FATAL_ERROR "Magics requires dlfcn.h for supporting the detection of the shared folder")
endif()

cmake_push_check_state(RESET)
    set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_DL_LIBS} )
    check_c_source_compiles( "#define _GNU_SOURCE\n#include <dlfcn.h>\nint main(){ void* addr; Dl_info info; dladdr(addr, &info); }\n"
        magics_HAVE_DLADDR )
cmake_pop_check_state()

endif()

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
    # For M_PI
    ecbuild_add_c_flags("/D_USE_MATH_DEFINES")
    ecbuild_add_cxx_flags("/D_USE_MATH_DEFINES")
    # Suppress C4267: warns about possible loss of data when converting 'size_t' to 'int'.
    ecbuild_add_c_flags("/wd4267")
    ecbuild_add_cxx_flags("/wd4267")
    # Suppress C4800: forcing value to bool 'true' or 'false'
    ecbuild_add_cxx_flags("/wd4800")
    # Suppress C4244: convertions from double to int
    #ecbuild_add_cxx_flags("/wd4244")
    # Suppress C4244: convertions from double to float
    #ecbuild_add_cxx_flags("/wd4305")

#### /D PROJ_MSVC_DLL_IMPORT

    # Prevent fatal error C1128
    ecbuild_add_cxx_flags("/bigobj")

else()

  # set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter -Wno-overloaded-virtual -Wno-unused-variable -Wno-reorder -Wno-sign-compare")
  ecbuild_add_cxx_flags("-Wno-unused-parameter" NO_FAIL)
  ecbuild_add_cxx_flags("-Wno-overloaded-virtual" NO_FAIL)
  ecbuild_add_cxx_flags("-Wno-unused-variable" NO_FAIL)
  ecbuild_add_cxx_flags("-Wno-reorder" NO_FAIL)
  ecbuild_add_cxx_flags("-Wno-sign-compare" NO_FAIL)
  # For NVIDIA
  ecbuild_add_cxx_flags("--diag_suppress=partial_override" NO_FAIL)
  ecbuild_add_cxx_flags("--diag_suppress=virtual_function_decl_hidden" NO_FAIL)
  ecbuild_add_cxx_flags("--diag_suppress=code_is_unreachable" NO_FAIL)

  # set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wno-unused-parameter -Wno-overloaded-virtual -Wno-unused-variable -Wno-reorder-ctor -Wno-sign-compare")

endif()

###############################################################################
# contents

if ( HAVE_ODB )
  ecbuild_info("Adding support for ODB")
  list( APPEND MAGICS_EXTRA_LIBRARIES odccore)
endif()



if( HAVE_CAIRO )
  list( APPEND MAGICS_PRIVATE_INCLUDE_DIRS ${PANGOCAIRO_INCLUDE_DIRS})

  if( EC_OS_NAME MATCHES "windows" )


    # Looks like the transitivity of includes is broken
    find_package(PkgConfig REQUIRED)

    pkg_search_module(GLIB REQUIRED glib-2.0)
    list( APPEND MAGICS_PRIVATE_INCLUDE_DIRS ${GLIB_INCLUDE_DIRS})

    pkg_search_module(CAIRO REQUIRED cairo)
    list( APPEND MAGICS_PRIVATE_INCLUDE_DIRS ${CAIRO_INCLUDE_DIRS})

    pkg_search_module(HARFBUZZ REQUIRED harfbuzz)
    list( APPEND MAGICS_PRIVATE_INCLUDE_DIRS ${HARFBUZZ_INCLUDE_DIRS})

  endif()

endif()

set( MAGICS_LIBRARIES MagPlus )

if( MAGICS_QT )
    if( MAGICS_QT6 )
        list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${Qt6Widgets_INCLUDE_DIR} )
        list( APPEND MAGICS_EXTRA_LIBRARIES     ${Qt6Widgets_LIBRARIES})
    elseif ( MAGICS_QT5 )
        list( APPEND MAGICS_EXTRA_INCLUDE_DIRS  ${Qt5Widgets_INCLUDE_DIR} )
        list( APPEND MAGICS_EXTRA_LIBRARIES     ${Qt5Widgets_LIBRARIES})
    endif()
    if( WITH_QT_DEBUG )
        list( APPEND MAGICS_EXTRA_DEFINITIONS QT_NO_DEBUG_OUTPUT )
    endif()
endif()

if( HAVE_GEOTIFF )
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS	${GEOTIFF_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES	${GEOTIFF_LIBRARY} )
	list( APPEND MAGICS_EXTRA_DEFINITIONS	HAVE_GEOTIFF )
endif()

if( HAVE_WEBP )
	list( APPEND MAGICS_EXTRA_INCLUDE_DIRS	${WEBP_INCLUDE_DIR} )
	list( APPEND MAGICS_EXTRA_LIBRARIES ${WEBP_LIBRARY} )
	list( APPEND MAGICS_EXTRA_DEFINITIONS	HAVE_WEBP )
endif()

list( APPEND MAGICS_EXTRA_LIBRARIES     ${CMAKE_THREAD_LIBS_INIT} )

if( EC_OS_NAME MATCHES "windows" )
    # We need to link to ws2_32.lib to avoid an unresolved external (gethostname)
    # It's part of the Windows SDK so no need to search for it.
    list( APPEND MAGICS_EXTRA_LIBRARIES ws2_32 )
    # We need to find zlib.lib
    # find_library( ZLIB_LIB zlib PATHS ENV PATH )
    # list( APPEND MAGICS_EXTRA_LIBRARIES ${ZLIB_LIB} )
    find_package( ZLIB REQUIRED )   # for MagML
    message("ZLIB include: ${ZLIB_INCLUDE_DIR}")
    message("ZLIB library: ${ZLIB_LIBRARIES}")
    list( APPEND MAGICS_EXTRA_LIBRARIES ${ZLIB_LIBRARIES} )
endif()

ecbuild_debug("MAGICS_EXTRA_DEFINITIONS  => ${MAGICS_EXTRA_DEFINITIONS}")
ecbuild_debug("MAGICS_EXTRA_INCLUDE_DIRS => ${MAGICS_EXTRA_INCLUDE_DIRS}")
ecbuild_debug("MAGICS_EXTRA_LIBRARIES    => ${MAGICS_EXTRA_LIBRARIES}")

get_directory_property( MAGICS_DEFINITIONS COMPILE_DEFINITIONS )

add_subdirectory( tools )
add_subdirectory( src )
add_subdirectory( share )
add_subdirectory( apps )
add_subdirectory( test )
if( HAVE_DOCUMENTATION )
  add_subdirectory( docs )
endif()


if(NOT HAVE_EFAS)
  ecbuild_add_resources( TARGET noefas DONT_PACK_DIRS share/magics/efas)
endif()

# Directories not need in the distribution tar ball!
ecbuild_add_resources( TARGET bamboo DONT_PACK_DIRS bamboo)
ecbuild_add_resources( TARGET old_resources
		DONT_PACK_DIRS
		docs
		regression
		toolsjs
		notebook)



############################################################################################
# finalize

set( MAGICS_DESCRIPTION "Multi-platform meteorological graphics library" )
set( MAGICS_URL "https://software.ecmwf.int/magics" )

### print all variables
#get_cmake_property(_variableNames VARIABLES)
#foreach (_variableName ${_variableNames})
#    ecbuild_info("${_variableName}=${${_variableName}}")
#endforeach()

ecbuild_install_project( NAME Magics )

ecbuild_print_summary()
