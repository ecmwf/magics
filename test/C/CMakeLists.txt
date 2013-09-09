

ecbuild_add_test ( TARGET magics_c
                   SOURCES magics.c
			   	   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} )

ecbuild_add_test ( TARGET magics_error
                   SOURCES error.c
			   	   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} )
