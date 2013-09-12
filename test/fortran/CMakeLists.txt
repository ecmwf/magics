
#file( DOWNLOAD http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib 
    #  ${CMAKE_CURRENT_BINARY_DIR}/msl.grib
    #  EXPECTED_HASH SHA1=da39a3ee5e6b4b0d3255bfef95601890afd80709 )
      
      
#ecbuild_get_resources ( LIST   http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib 0 )

ecbuild_add_test( TARGET    magics_fortran
                  SOURCES   magics.f
                  LIBS      MagPlusSingle MagPlus
                  LINKER_LANGUAGE Fortran )

#ecbuild_add_test( TARGET magics_fortran_grib
        #SOURCES grib.f90 
        #RESOURCES msl.grib 
        #LIBS MagPlusSingle MagPlus 
       # LINKER_LANGUAGE Fortran)
