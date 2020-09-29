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

      set( ouputfiles ${CMAKE_CURRENT_SOURCE_DIR}/attributes/${basefile}Attributes.h ${CMAKE_CURRENT_SOURCE_DIR}/attributes/${basefile}Attributes.cc )
      list( APPEND magics_params_srcs ${ouputfiles} )
endforeach()


foreach( file ${metview_xmls} )
      get_filename_component( path     ${file} PATH   )
         get_filename_component( basefile ${file} NAME_WE )

      set( metviewfiles ${CMAKE_CURRENT_SOURCE_DIR}/attributes/${basefile}Wrapper.h ${CMAKE_CURRENT_SOURCE_DIR}/attributes/${basefile}Wrapper.cc )
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



#add_custom_command( magics_params DEPENDS ${magics_params_srcs} ${metview_wrappers_srcs} )

###############################################################################

add_subdirectory( common )
add_subdirectory( basic )
add_subdirectory( web )
add_subdirectory( visualisers )
add_subdirectory( drivers )
add_subdirectory( decoders )
add_subdirectory( terralib )
add_subdirectory( clipper )
add_subdirectory( eckit_readers )

if( metview )
	add_subdirectory( libMagWrapper )
endif()

if( HAVE_ODB )
	add_subdirectory( oda )
  # Adding path to the Oda directory 
  list( APPEND MAGICS_PRIVATE_INCLUDE_DIRS $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/oda> )
endif()

ecbuild_add_library( TARGET    MagPlus
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
                        ${clipper_srcs}
                        ${terralib_srcs}
                        ${odb_srcs}
                     TEMPLATES
                        ${common_templates}
                     PUBLIC_DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
                     PUBLIC_LIBS
                        ${MAGICS_EXTRA_LIBRARIES} eccodes   expat ${PROJ4_LIBRARIES}
                     PRIVATE_INCLUDES $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/clipper>
                                      ${MAGICS_PRIVATE_INCLUDE_DIRS}
                                     
                     PUBLIC_INCLUDES $<INSTALL_INTERFACE:${INSTALL_INCLUDE_DIR}>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/attributes>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/basic>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/common>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/drivers>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/decoders>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/visualisers>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/terralib/kernel>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/eckit_readers>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/web>
                                     $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/drivers/MgQ>
                                     $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
                     TYPE SHARED )

target_link_libraries(MagPlus PRIVATE ${PROJ4_LIBRARIES} ${PROJ_LIBRARIES})

if(APPLE)
  target_link_libraries(MagPlus PRIVATE ${ZLIB_LIBRARIES})
endif(APPLE)

if(HAVE_CAIRO)
    target_link_libraries(MagPlus PRIVATE ${PANGOCAIRO_LIBRARIES})
endif()

if(HAVE_NETCDF)
    target_link_libraries(MagPlus PRIVATE NetCDF::NetCDF_C)
endif()

ecbuild_add_library( TARGET         MagPlusSingle
                     SOURCES        MagicsSingle.cc
                     PRIVATE_LIBS   MagPlus
                     TYPE           SHARED )

ecbuild_add_library( TARGET         MagPlusDouble
                     SOURCES        MagicsDouble.cc
                     PRIVATE_LIBS   MagPlus
                     TYPE           SHARED )

if( metview )

   ecbuild_add_library(	TARGET			MagWrapper
                     	SOURCES			magics.h
                        			${metview_srcs}
                        			${metview_wrappers_srcs}
                     	PUBLIC_DEFINITIONS	${MAGICS_EXTRA_DEFINITIONS}
                     	PUBLIC_LIBS		MagPlus
                        PRIVATE_INCLUDES ${MAGICS_PRIVATE_INCLUDE_DIRS}
                        PUBLIC_INCLUDES $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/libMagWrapper>
                     	TYPE			STATIC
                    )
   if(HAVE_CAIRO)
    target_link_libraries(MagWrapper PRIVATE ${PANGOCAIRO_LIBRARIES})
   endif()

   if(HAVE_NETCDF)
    target_link_libraries(MagWrapper PRIVATE NetCDF::NetCDF_C)
   endif()

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
            LookupTableModeAttributes.h
            FixedTableModeAttributes.h
   )
   if(qt)
     list(APPEND attributes_include QtDriverAttributes.h)
   endif()

   install( FILES terralib/kernel/TeProjection.h
		  terralib/kernel/TeCoord2D.h
		  terralib/kernel/TeDefines.h
		  terralib/kernel/TePrecision.h
		  terralib/kernel/TeSingleton.h
		  terralib/kernel/TeDatum.h
		  terralib/kernel/TeUtils.h
		  DESTINATION ${INSTALL_INCLUDE_DIR})

   install( FILES AutoVector.h       DESTINATION ${INSTALL_INCLUDE_DIR})
   install( FILES Iterable.h         DESTINATION ${INSTALL_INCLUDE_DIR})
   install( FILES ReverseIterable.h  DESTINATION ${INSTALL_INCLUDE_DIR})

   foreach( file ${attributes_include} )
     install( FILES ${CMAKE_CURRENT_SOURCE_DIR}/attributes/${file}  DESTINATION ${INSTALL_INCLUDE_DIR})
   endforeach()
   install( FILES ${metview_include} DESTINATION ${INSTALL_INCLUDE_DIR})
endif()


install( FILES magics.h                DESTINATION ${INSTALL_INCLUDE_DIR})
install( FILES common/magics_export.h  DESTINATION ${INSTALL_INCLUDE_DIR})
install( FILES common/magics_windef.h  DESTINATION ${INSTALL_INCLUDE_DIR})

## Add install include dirs to install interface
foreach( target     MagPlus
                    MagPlusStatic
                    MagPlusSingle
                    MagPlusSingleStatic
                    MagPlusDouble
                    MagPlusDoubleStatic
                    MagWrapper )
    if( TARGET ${target} AND DEFINED ecbuild_VERSION)
        target_include_directories( ${target} INTERFACE $<INSTALL_INTERFACE:${INSTALL_INCLUDE_DIR}> )
    endif()
endforeach()

