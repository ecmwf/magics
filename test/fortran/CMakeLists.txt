#file( DOWNLOAD http://www.cmake.org/files/v2.8/cmake-2.8.11.2.tar.gz ${CMAKE_CURRENT_BINARY_DIR}/cmake.tar.gz
#      EXPECTED_HASH SHA1=31f217c9305add433e77eff49a6eac0047b9e929 )

file( DOWNLOAD http://download.ecmwf.int/test-data/magics/regression_test/examples/cyclone/msl_11P_CYRIL_2012020712 ${CMAKE_CURRENT_BINARY_DIR}/msl_11P_CYRIL_2012020712
      EXPECTED_HASH SHA1=c91c41d052e4b7421a819a331ab060b27e0764f3 )

ecbuild_add_test( TARGET    magics_fortran
                  SOURCES   magics.f
                  LIBS      MagPlusSingle MagPlus
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET magics_fortran_grib
        SOURCES grib.f90 
        RESOURCES msl.grib 
        LIBS MagPlusSingle MagPlus 
        LINKER_LANGUAGE Fortran)
