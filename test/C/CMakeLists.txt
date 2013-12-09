# magics c test

ecbuild_add_test(  TARGET magics_c
                   SOURCES magics.c
		   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} )

# magics test executable run in script

ecbuild_add_test(  TARGET   script.x
                   SOURCES  somex.c
                   TYPE     SCRIPT
                   COMMAND  ${CMAKE_CURRENT_SOURCE_DIR}/myecho.sh  ARGS  ./script.x )
