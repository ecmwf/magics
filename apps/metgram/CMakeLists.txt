# Copy metgram executable to the build tree
configure_file( metgram  metgram COPYONLY )
configure_file( metgram-script.in  metgram.sh @ONLY )

install ( DIRECTORY templates DESTINATION ${INSTALL_DATA_DIR}/.. FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ)


install ( FILES ${CMAKE_CURRENT_SOURCE_DIR}/metgram  ${CMAKE_CURRENT_BINARY_DIR}/metgram.sh 
			DESTINATION ${INSTALL_BIN_DIR} 
			PERMISSIONS OWNER_READ GROUP_READ WORLD_READ 
				        OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)


ecbuild_add_executable( TARGET    metgramx
                     SOURCES
                        metgramx.cc
                     DEFINITIONS
                        ${MAGICS_EXTRA_DEFINITIONS}
                    LIBS
                        MagPlus ${MAGICS_EXTRA_LIBRARIES}
                    )
