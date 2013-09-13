
if(0)
	message(status "DOWNLOAD http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib into ${CMAKE_CURRENT_BINARY_DIR}/msl.grib")

	file( DOWNLOAD http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib 
		${CMAKE_CURRENT_BINARY_DIR}/msl.grib
		EXPECTED_HASH SHA1=da39a3ee5e6b4b0d3255bfef95601890afd80709 )

	message(status "Run: perl -e 'print -s q(${CMAKE_CURRENT_BINARY_DIR}/msl.grib);'")

	execute_process(
		COMMAND "perl" "-e" "print -s q(${CMAKE_CURRENT_BINARY_DIR}/msl.grib);"
		RESULT_VARIABLE code
		OUTPUT_VARIABLE size)

	message(status "  ---> result=${code}, output=${size}")

	if (NOT ${code})
		if (NOT ${size}) 
			#message(FATAL_ERROR "${CMAKE_CURRENT_BINARY_DIR}/msl.grib is empty")
			message(WARNING "${CMAKE_CURRENT_BINARY_DIR}/msl.grib is empty")
		endif()
	else()
		message(FATAL_ERROR "perl ${_args} failed with code ${code}")
	endif()
endif()

function (ecbuild_get_test_data NAME FROM)
	# TODO: choose between wget and curl

	add_custom_command(
		OUTPUT ${NAME}
		COMMAND wget -O ${NAME} http://download.ecmwf.int/test-data/${FROM}/${NAME}
	)

endfunction(ecbuild_get_test_data)

ecbuild_get_test_data(msl.grib magics/test/fortran)

#add_custom_command(
#	OUTPUT msl.grib
#	COMMAND wget -O msl.grib http://download.ecmwf.int/test-data/magics/test/fortran/msl.grib
#)

ecbuild_add_test( TARGET    magics_fortran
	SOURCES   magics.f
	LIBS      MagPlusSingle MagPlus
	DEPENDS   msl.grib
	LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET magics_fortran_grib
	SOURCES grib.f90 
	LIBS MagPlusSingle MagPlus 
	DEPENDS   msl.grib
	LINKER_LANGUAGE Fortran)
