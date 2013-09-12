file( DOWNLOAD http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib 
	${CMAKE_CURRENT_BINARY_DIR}/msl.grib
	EXPECTED_HASH SHA1=da39a3ee5e6b4b0d3255bfef95601890afd80709 )

set(_q "\"")
set(_args "-e ${_q}print -s '${CMAKE_CURRENT_BINARY_DIR}/msl.grib';${_q} -w")

message(status "Run: perl ${_args}")

execute_process(COMMAND "perl" ${_args}
	RESULT_VARIABLE _result
	OUTPUT_VARIABLE _size)

message(status "  ---> result=${_result}, output=${_size}")

if (_result equal 0)
	if (_size equal 0) 
		message(FATAL_ERROR "${CMAKE_CURRENT_BINARY_DIR}/msl.grib is empty")
	endif()
else()
	message(FATAL_ERROR "perl ${_args} failed with code ${_result}")
endif()


ecbuild_add_test( TARGET    magics_fortran
	SOURCES   magics.f
	LIBS      MagPlusSingle MagPlus
	LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET magics_fortran_grib
	SOURCES grib.f90 
	LIBS MagPlusSingle MagPlus 
	LINKER_LANGUAGE Fortran)
