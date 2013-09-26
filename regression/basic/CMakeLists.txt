
# magics test executable run in script
# download data 
ecbuild_get_test_data(data.grib magics/regression/basic 0)

ecbuild_get_test_data(ref_basic.png magics/reference/${MAGICS_REFERENCE_VERSION}/basic 0)


# download reference
ecbuild_add_test(  TARGET   magics_basicc
                   SOURCES  basic.c
                   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} 
                   TYPE     SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  ARGS  ./magics_basicc basic.png ref_basic.png 20 )
ADD_DEPENDENCIES(magics_basicc test_data_data_grib test_data_ref_basic_png)

# download reference
#ecbuild_add_test(  TARGET   magics_basicf90
                 #  SOURCES  basic.f90
                 #  LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} 
                 #  TYPE     SCRIPT
                #   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  ARGS  ./magics_basicf90 basicf90.png ref_basic.png 20 )


#ADD_DEPENDENCIES(magics_basicf90 test_data_data_grib test_data_ref_basic_png)
