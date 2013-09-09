

ecbuild_add_test ( TARGET fortran_magics
        SOURCES magics.f 
        LIBS MagPlusSingle MagPlus 
        LINKER_LANGUAGE Fortran)

ecbuild_add_test ( TARGET fortran_grib
        SOURCES grib.f90 
        RESOURCES msl.grib 
        LIBS MagPlusSingle MagPlus 
        LINKER_LANGUAGE Fortran)
