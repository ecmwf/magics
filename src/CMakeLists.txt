###############################################################################

configure_file( magics_config.h.in  magics_config.h  )

###############################################################################

add_subdirectory( params )

foreach( file ${params_xmls} )

      get_filename_component( path     ${file} PATH   )
      get_filename_component( basefile ${file} NAME_WE )

      set( ouputfiles ${path}/${basefile}Attributes.h ${path}/${basefile}Attributes.cc )

      # generate the code
      add_custom_command( 
            OUTPUT  ${ouputfiles}
            COMMAND ${PERL_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tools/xml2cc_new.pl ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/params nosubdir
            DEPENDS ${file}
      )

      set_source_files_properties( ${basefile}Attributes.h  GENERATED )
      set_source_files_properties( ${basefile}Attributes.cc GENERATED )

      list( APPEND params_srcs ${ouputfiles} )

endforeach()

add_custom_target( all_params DEPENDS ${params_srcs} )

###############################################################################

add_subdirectory( common )
add_subdirectory( basic )
add_subdirectory( web )
add_subdirectory( visualisers )
add_subdirectory( drivers )
add_subdirectory( decoders )
add_subdirectory( terralib )

ecbuild_add_library( TARGET    MagPlusSingle 
		     SOURCES MagicsSingle.cc
		     INCLUDES  ${CMAKE_CURRENT_SOURCE_DIR}/common
		     TYPE STATIC)
		     
ecbuild_add_library( TARGET    MagPlusDouble 
		     SOURCES MagicsDouble.cc
		     INCLUDES  ${CMAKE_CURRENT_SOURCE_DIR}/common
		     TYPE STATIC)		     
		     
ecbuild_add_library( TARGET    MagPlus
                     DEPENDS   all_params
                     SOURCES
                        magics.h
                        ${params_srcs} 
                        ${common_srcs} 
                        ${basic_srcs}
                        ${web_srcs}
                        ${visualisers_srcs}
                        ${drivers_srcs}
                        ${decoders_srcs}
                        ${terralib_srcs}
                     TEMPLATES
                        ${common_templates}
                     DEFINITIONS
                        ${GRIB_API_DEFINITIONS}
                     INCLUDES  
                        ${CMAKE_CURRENT_SOURCE_DIR} 
                        ${CMAKE_CURRENT_BINARY_DIR} 
                        ${CMAKE_CURRENT_BINARY_DIR}/params 
                        ${CMAKE_CURRENT_SOURCE_DIR}/common  
                        ${CMAKE_CURRENT_SOURCE_DIR}/basic
                        ${CMAKE_CURRENT_SOURCE_DIR}/web
                        ${CMAKE_CURRENT_SOURCE_DIR}/visualisers
                        ${CMAKE_CURRENT_SOURCE_DIR}/drivers
                        ${CMAKE_CURRENT_SOURCE_DIR}/decoders
                        ${CMAKE_CURRENT_SOURCE_DIR}/terralib
                        ${CMAKE_CURRENT_SOURCE_DIR}/terralib/kernel
                        ${CMAKE_CURRENT_SOURCE_DIR}/terralib/utils
                        ${Boost_INCLUDE_DIRS}
                        ${NETCDF_INCLUDE_DIRS}
                        ${Cairo_INCLUDE_DIRS}
                        ${PROJ4_INCLUDE_DIRS}
                        ${GRIB_API_INCLUDE_DIRS}
                        ${ECLIB_INCLUDE_DIRS}
                    LIBS
                        ${GRIB_API_LIBRARIES}
                        ${ECLIB_LIBRARIES} 
                        ${Cairo_LIBRARIES} 
                        ${NETCDF_LIBRARIES} 
                        ${PROJ4_LIBRARIES}
                        ${QT_LIBRARIES} 
                    )
