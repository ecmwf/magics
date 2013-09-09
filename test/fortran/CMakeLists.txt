ecbuild_get_resources( LIST
http://download.ecmwf.int/test-data/magics/regression_test/examples/cyclone/msl_11P_CYRIL_2012020712 c91c41d052e4b7421a819a331ab060b27e0764f3
)

ecbuild_add_test( TARGET    magics_fortran
                  SOURCES   magics.f
                  LIBS      MagPlusSingle MagPlus
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET magics_fortran_grib
        SOURCES grib.f90 
        RESOURCES msl.grib 
        LIBS MagPlusSingle MagPlus 
        LINKER_LANGUAGE Fortran)
