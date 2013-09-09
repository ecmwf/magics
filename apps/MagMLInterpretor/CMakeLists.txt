

configure_file( magics-script.in  magjson @ONLY )
configure_file( magics-script.in  magml @ONLY )

install ( FILES ${CMAKE_CURRENT_BINARY_DIR}/magjson 
			DESTINATION ${INSTALL_BIN_DIR} 
			PERMISSIONS OWNER_READ GROUP_READ WORLD_READ 
				        OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)

install ( FILES ${CMAKE_CURRENT_BINARY_DIR}/magml 
			DESTINATION ${INSTALL_BIN_DIR} 
			PERMISSIONS OWNER_READ GROUP_READ WORLD_READ 
				        OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)

ecbuild_add_executable( TARGET    magjsonx
     
                     SOURCES
                        json.cc
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
               
                    LIBS
                        MagPlus ${MAGICS_EXTRA_LIBRARIES}
                    )
                    
ecbuild_add_executable( TARGET    magmlx
                     SOURCES
                        magml.cc
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
					 LIBS
                        MagPlus ${MAGICS_EXTRA_LIBRARIES}
                    )
