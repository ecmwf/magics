# magics test executable run in script

# download data 
ecbuild_get_test_data( NAME data.grib NOCHECK )

# download reference
ecbuild_get_test_data( NAME ref_basic.png NOCHECK DIRNAME magics/reference/${MAGICS_REFERENCE_VERSION}/basic )


ecbuild_add_test(  TARGET   magics_basicc
                   SOURCES  basic.c
                   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} 
                   DEPENDS  test_data_data_grib test_data_ref_basic_png
                   TYPE     SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  ARGS  ./magics_basicc basic.png ref_basic.png 20 )

#ecbuild_add_test(  TARGET   magics_basicf90
                 #  SOURCES  basic.f90
                 #  LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} 
                 #  DEPENDS  test_data_data_grib test_data_ref_basic_png
                 #  TYPE     SCRIPT
                 #   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  ARGS  ./magics_basicf90 basicf90.png ref_basic.png 20 )


