###############################################################################

configure_file( magics_config.h.in  magics_config.h  )

###############################################################################

add_subdirectory( params )

foreach( file ${magics_xmls} )

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

      list( APPEND magics_params_srcs ${ouputfiles} )

endforeach()
     
if ( WITH_METVIEW )      
	foreach( file ${metview_xmls} )
	      get_filename_component( path     ${file} PATH   )
          get_filename_component( basefile ${file} NAME_WE )
		  
		  set( metviewfiles ${path}/${basefile}Wrapper.h ${path}/${basefile}Wrapper.cc )
		  
		  
		# generate the code
		  add_custom_command( 
				OUTPUT  ${metviewfiles}
				COMMAND ${PERL_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tools/xml2mv.pl ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/params nosubdir
				DEPENDS ${file}
			)

		  set_source_files_properties( ${basefile}Wrapper.h  GENERATED )
          set_source_files_properties( ${basefile}Wrapper.cc GENERATED )
          list( APPEND metview_wrappers_srcs ${metviewfiles} )
      endforeach()
      
      SET(qt_drivers_HEADERS drivers/MgQ/MgQPlotScene.h)
     QT4_WRAP_CPP(qt_drivers_HEADERS_MOC ${qt_drivers_HEADERS})
     debug_var ( qt_drivers_HEADERS_MOC )
     list( APPEND drivers_srcs  drivers/MgQ/MgQPlotScene.cc  ${qt_drivers_HEADERS_MOC}) 

endif()
	 






add_custom_target( magics_params DEPENDS ${magics_params_srcs} )
add_custom_target( mv_params DEPENDS ${metview_wrappers_srcs} )
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
                     DEPENDS   magics_params
                     SOURCES
                        magics.h
                        ${magics_params_srcs} 
                        ${common_srcs} 
                        ${basic_srcs}
                        ${web_srcs}
                        ${visualisers_srcs}
                        ${drivers_srcs}
                        ${qt_srcs}
                        ${decoders_srcs}
                        ${terralib_srcs} 
                        ${odb_srcs}
                     TEMPLATES
                        ${common_templates}
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
                    LIBS
                        ${MAGICS_EXTRA_LIBRARIES}
                    )

if ( WITH_METVIEW ) 
  ecbuild_add_library( TARGET    MagWrapper
                     DEPENDS   mv_params
                     SOURCES
                        magics.h
                        ${metview_srcs} 
                        ${metview_wrappers_srcs} 
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
