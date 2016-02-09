###############################################################################

configure_file( magics_config.h.in  magics_config.h  )

install ( FILES ${CMAKE_CURRENT_BINARY_DIR}/magics_config.h  DESTINATION ${INSTALL_INCLUDE_DIR})

###############################################################################


function( add_metview_definition_file )
   
    set( single_value_args ACTION BASENAME FILE ) 
    cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )
     
    add_custom_command( 
            OUTPUT ${_p_BASENAME}Def ${_p_BASENAME}Rules
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/../../tools/xml2mv.py ${CMAKE_CURRENT_SOURCE_DIR} ${_p_FILE}  ${_p_BASENAME}Def ${_p_ACTION} ${_p_BASENAME}Rules
            DEPENDS ${_p_FILE}
      )
     
     
    add_custom_target( ${_p_BASENAME} ALL DEPENDS ${_p_BASENAME}Def)
    install ( FILES ${CMAKE_CURRENT_BINARY_DIR}/${_p_BASENAME}Def ${CMAKE_CURRENT_BINARY_DIR}/${_p_BASENAME}Rules DESTINATION ${INSTALL_DATA_DIR}/../magics/metview)

endfunction(add_metview_definition_file)

add_subdirectory( params )



foreach( file ${magics_xmls} )

      get_filename_component( path     ${file} PATH   )
      get_filename_component( basefile ${file} NAME_WE )

      set( ouputfiles ${path}/${basefile}Attributes.h ${path}/${basefile}Attributes.cc )
      
      # generate the code
      add_custom_command( 
            OUTPUT  ${ouputfiles}
            COMMAND ${PERL_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/../tools/xml2cc_new.pl ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/params nosubdir
            DEPENDS ${file}
      )

      set_source_files_properties( ${basefile}Attributes.h  GENERATED )
      set_source_files_properties( ${basefile}Attributes.cc GENERATED )

      list( APPEND magics_params_srcs ${ouputfiles} )

endforeach()
     
if ( metview )

	foreach( file ${metview_xmls} )
	      get_filename_component( path     ${file} PATH   )
          get_filename_component( basefile ${file} NAME_WE )
		  
		  set( metviewfiles ${path}/${basefile}Wrapper.h ${path}/${basefile}Wrapper.cc )
		  
		  
		# generate the code
		  add_custom_command( 
				OUTPUT  ${metviewfiles}
				COMMAND ${PERL_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/../tools/xml2mv.pl ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/params nosubdir
				DEPENDS ${file}
			)

		  set_source_files_properties( ${basefile}Wrapper.h  GENERATED )
          set_source_files_properties( ${basefile}Wrapper.cc GENERATED )
          list( APPEND metview_wrappers_srcs ${metviewfiles} )
      endforeach()

     if(qt)
        SET(qt_drivers_HEADERS drivers/MgQ/MgQPlotScene.h)
        if( MAGICS_QT5 )
          QT5_WRAP_CPP(qt_drivers_HEADERS_MOC ${qt_drivers_HEADERS})
        else()
          QT4_WRAP_CPP(qt_drivers_HEADERS_MOC ${qt_drivers_HEADERS})
        endif()        
        list( APPEND drivers_srcs  drivers/MgQ/MgQPlotScene.cc  ${qt_drivers_HEADERS_MOC})
     endif()
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
add_subdirectory( eckit_readers )

if( metview )
	add_subdirectory( libMagWrapper )
endif()

if( HAVE_ODB )
	add_subdirectory( oda )
endif()

include_directories( ${CMAKE_CURRENT_SOURCE_DIR}/common )

ecbuild_add_library( TARGET    MagPlus
                     DEPENDS   magics_params
                     SOURCES
                        magics.h
                        ${magics_params_srcs} 
                        ${common_srcs} 
                        ${basic_srcs}
                        ${web_srcs}
                        ${visualisers_srcs}
                        ${eckit_srcs}
                        ${drivers_srcs}
                        ${qt_srcs}
                        ${decoders_srcs}
                        ${terralib_srcs} 
                        ${odb_srcs}
                     TEMPLATES
                        ${common_templates}
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS} MAGICS_EXCEPTION=on
                     LIBS
                        ${MAGICS_EXTRA_LIBRARIES}
                     TYPE SHARED )

ecbuild_add_library( TARGET      MagPlusSingle
                     SOURCES     MagicsSingle.cc
                     LIBS        MagPlus
                     TYPE        SHARED )

ecbuild_add_library( TARGET      MagPlusDouble
                     SOURCES     MagicsDouble.cc
                     LIBS        MagPlus
                     TYPE        SHARED )

if ( BUILD_SHARED_LIBS STREQUAL "BOTH" )
ecbuild_add_library( TARGET    MagPlusStatic
                     DEPENDS   magics_params
                     SOURCES
                        magics.h
                        ${magics_params_srcs} 
                        ${common_srcs} 
                        ${basic_srcs}
                        ${web_srcs}
                        ${visualisers_srcs}
                        ${eckit_srcs}
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
                     TYPE STATIC
                     OUTPUT_NAME MagPlus )

ecbuild_add_library( TARGET      MagPlusSingleStatic 
                     SOURCES     MagicsSingle.cc
                     LIBS        MagPlusStatic
                     TYPE        STATIC
                     OUTPUT_NAME MagPlusSingle )

ecbuild_add_library( TARGET      MagPlusDoubleStatic
                     SOURCES     MagicsDouble.cc
                     LIBS        MagPlusStatic
                     TYPE        STATIC
                     OUTPUT_NAME MagPlusDouble )

endif()

if( metview )
 include_directories(  ${CMAKE_CURRENT_SOURCE_DIR} 
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
                        ${CMAKE_CURRENT_SOURCE_DIR}/libMagWrapper )
  

                        
  ecbuild_add_library( TARGET    MagWrapper
                     DEPENDS   mv_params magics_params
                     SOURCES
                        magics.h
                        ${metview_srcs} 
                        ${metview_wrappers_srcs} 
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}                   
                    LIBS
                        ${MAGICS_EXTRA_LIBRARIES}
                    TYPE STATIC		
                    )
   list( APPEND attributes_include
				OutputHandlerAttributes.h 
				FortranRootSceneNodeAttributes.h  
                BinningObjectAttributes.h
                GribDecoderAttributes.h
                DateGribLoopStepAttributes.h
                GribLoopAttributes.h
                GeoRectangularProjectionAttributes.h
                LegendVisitorAttributes.h
				XmlRootNodeAttributes.h 
				WrepRootNodeAttributes.h
                ContinuousLegendMethodAttributes.h
                HistogramLegendMethodAttributes.h
                XmlBasicNodeAttributes.h
                ImagePlottingAttributes.h
                LookupTableModeAttributes.h
                FixedTableModeAttributes.h
				)
   if(qt)
     list(APPEND attributes_include
                 QtDriverAttributes)
   endif()

   install( FILES terralib/kernel/TeProjection.h
				   terralib/kernel/TeCoord2D.h 
				   terralib/kernel/TeDefines.h 
				   terralib/kernel/TePrecision.h 
				   terralib/kernel/TeSingleton.h 
				   terralib/kernel/TeDatum.h 
				   terralib/kernel/TeUtils.h
				   terralib/kernel/TeProgress.h
				   terralib/kernel/TeProgressBase.h
				   
				   
				   DESTINATION ${INSTALL_INCLUDE_DIR})
   install( FILES VectorOfPointers.h  DESTINATION ${INSTALL_INCLUDE_DIR})
   foreach( file ${attributes_include} )
     install( FILES ${CMAKE_CURRENT_BINARY_DIR}/params/${file}  DESTINATION ${INSTALL_INCLUDE_DIR})
   endforeach()
   install( FILES ${metview_include} DESTINATION ${INSTALL_INCLUDE_DIR})
endif()
install( FILES magics.h  DESTINATION ${INSTALL_INCLUDE_DIR})
