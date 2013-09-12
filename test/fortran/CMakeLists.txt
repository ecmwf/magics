file( DOWNLOAD http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib 
	${CMAKE_CURRENT_BINARY_DIR}/msl.grib
	EXPECTED_HASH SHA1=da39a3ee5e6b4b0d3255bfef95601890afd80709 )

set(_cmd "/usr/bin/perl -e 'print -s q(${CMAKE_CURRENT_BINARY_DIR}/msl.grib);'")

message(status "Run: ${_cmd}")

execute_process(COMMAND ${_cmd}
	RESULT_VARIABLE _result
	OUTPUT_VARIABLE _size)

message(status "  ---> result=${_result}, output=${_size}")

if (_result equal 0)
	if (_size equal 0) 
		message(FATAL_ERROR "${CMAKE_CURRENT_BINARY_DIR}/msl.grib is empty")
	endif()
else()
	message(FATAL_ERROR "${_cmd} failed")
endif()


ecbuild_add_test( TARGET    magics_fortran
	SOURCES   magics.f
	LIBS      MagPlusSingle MagPlus
	LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET magics_fortran_grib
	SOURCES grib.f90 
	LIBS MagPlusSingle MagPlus 
	LINKER_LANGUAGE Fortran)
