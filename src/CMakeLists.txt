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
      
     
      if ( WITH_METVIEW ) 
         
		  set( metviewfiles ${path}/${basefile}Wrapper.h ${path}/${basefile}Wrapper.cc )
		  
		  
		# generate the code
		  add_custom_command( 
				OUTPUT  ${metviewfiles}
				COMMAND ${PERL_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tools/xml2mv.pl ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/params nosubdir
				DEPENDS ${file}
			)

		  set_source_files_properties( ${basefile}Wrapper.h  GENERATED )
          set_source_files_properties( ${basefile}Wrapper.cc GENERATED )
          list( APPEND wrappers_srcs ${metviewfiles} )
	  endif()
	  
endforeach()

add_custom_target( all_params DEPENDS ${params_srcs} )
add_custom_target( mv_params DEPENDS ${wrappers_srcs} )
###############################################################################

add_subdirectory( common )
add_subdirectory( basic )
add_subdirectory( web )
add_subdirectory( visualisers )
add_subdirectory( drivers )
add_subdirectory( decoders )
add_subdirectory( terralib )

if ( WITH_METVIEW ) 
	add_subdirectory( libMagWrapper )
endif()

if ( MAGICS_ODB ) 
	add_subdirectory( oda )
endif()

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
                        ${odb_srcs}
                     TEMPLATES
                        ${common_templates}
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
                     INCLUDES  
                        ${CMAKE_CURRENT_SOURCE_DIR} 
                        ${CMAKE_CURRENT_BINARY_DIR} 
                        ${CMAKE_CURRENT_BINARY_DIR}/params 
                        ${CMAKE_CURRENT_SOURCE_DIR}/common  
                        ${CMAKE_CURRENT_SOURCE_DIR}/basic
                        ${CMAKE_CURRENT_SOURCE_DIR}/web
                        ${CMAKE_CURRENT_SOURCE_DIR}/visualisers
                        ${CMAKE_CURRENT_SOURCE_DIR}/drivers
                        ${CMAKE_CURRENT_SOURCE_DIR}/drivers/MgQ
                        ${CMAKE_CURRENT_SOURCE_DIR}/decoders
                        ${CMAKE_CURRENT_SOURCE_DIR}/terralib
                        ${CMAKE_CURRENT_SOURCE_DIR}/terralib/kernel
                        ${CMAKE_CURRENT_SOURCE_DIR}/terralib/utils
                    LIBS
                        ${MAGICS_EXTRA_LIBRARIES}
                    )

if ( WITH_METVIEW ) 
  ecbuild_add_library( TARGET    MagWrapper
                     DEPENDS   mv_params
                     SOURCES
                        magics.h
                        ${metview_srcs} 
                        ${wrappers_srcs} 
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
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
                        ${CMAKE_CURRENT_SOURCE_DIR}/libMagWrapper
                    LIBS
                        ${MAGICS_EXTRA_LIBRARIES}
                    TYPE STATIC		
                    )
endif()
