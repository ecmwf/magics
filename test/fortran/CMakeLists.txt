
ecbuild_get_test_data(msl.grib magics/test/fortran f69ca0929d1122c7878d19f32401abe9)

ecbuild_add_test( TARGET    magics_fortran
	SOURCES   magics.f
	LIBS      MagPlusSingle MagPlus
	LINKER_LANGUAGE CXX)

ecbuild_add_test( TARGET magics_fortran_grib
	SOURCES grib.f90 
	LIBS MagPlusSingle MagPlus 
	LINKER_LANGUAGE CXX)

ADD_DEPENDENCIES(magics_fortran_grib test_data_msl_grib)

