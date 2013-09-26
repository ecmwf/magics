# magics test executable run in script

# download data 
ecbuild_get_test_data( NAME data.grib NOCHECK )

# download reference
ecbuild_get_test_data( NAME ref_basic.png NOCHECK 
				DIRNAME magics/reference/${MAGICS_REFERENCE_VERSION}/basic )


# Run Test
ecbuild_add_test(  TARGET   magics_a_basic
                   TYPE     SCRIPT
                   SOURCES  basic.c
                   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} 
                   DEPENDS  test_data_data_grib test_data_ref_basic_png
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  
                   ARGS  ./magics_a_basic ref_basic.png 20 ${MAGICS_REFERENCE_VERSION} ${MAGICS_HTML_ROOT})


# contour
ecbuild_get_test_data( NAME z500.grb NOCHECK  )
ecbuild_get_test_data( NAME ref_contour.ps NOCHECK
				DIRNAME magics/reference/${MAGICS_REFERENCE_VERSION}/basic )
						
# Run Test
ecbuild_add_test(  TARGET   magics_a_contour
                   TYPE     SCRIPT
                   SOURCES  contour.f
                   LINKER_LANGUAGE CXX
                   LIBS MagPlus MagPlusSingle ${MAGICS_EXTRA_LIBRARIES}
                   DEPENDS  test_data_z500_grb test_data_ref_contour_ps
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   ARGS  ./magics_a_contour ref_contour.ps 20 ${MAGICS_REFERENCE_VERSION} ${MAGICS_HTML_ROOT})

## Symbol using python
ecbuild_get_test_data( NAME ref_symbol.png NOCHECK
                       DIRNAME magics/reference/${MAGICS_REFERENCE_VERSION}/basic )


# Run Test
ecbuild_add_test(  TARGET  magics_a_symbol
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   DEPENDS test_data_ref_symbol_png
                   RESOURCES symbol.py
                   ARGS symbol.py ref_contour.png 20 ${MAGICS_REFERENCE_VERSION} ${MAGICS_HTML_ROOT}
                   ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )


