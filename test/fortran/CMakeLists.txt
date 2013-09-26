ecbuild_add_test( TARGET    magics_fortran
	SOURCES   magics.f
	LIBS      MagPlusSingle MagPlus
	LINKER_LANGUAGE CXX)


ecbuild_add_test( TARGET magics_fortran_grib
    SOURCES   grib.f90
    LIBS      MagPlusSingle MagPlus
    TEST_DATA msl.grib
    LINKER_LANGUAGE CXX )

