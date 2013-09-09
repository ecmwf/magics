



ecbuild_add_executable( TARGET    magjson
     
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
