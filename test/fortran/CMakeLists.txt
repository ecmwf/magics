# TODO: Move to ecbuild

function(_get_test_data basename dirname)
	# TODO: choose between wget and curl
	add_custom_command(
		OUTPUT ${basename}
		COMMAND env no_proxy= NO_PROXY= http_proxy=http://proxy.ecmwf.int:3333 
			curl --silent --show-error --output ${basename} http://download.ecmwf.int/test-data/${dirname}/${basename})
endfunction()


function (ecbuild_get_test_data basename dirname md5)

    STRING(REGEX REPLACE "[^A-Za-z0-9_]" "_"
		TARGET "test_data_${basename}")

	_get_test_data(${basename} ${dirname})
	set(depends ${basename})

	if(MD5) 
		add_custom_command(
			OUTPUT ${basename}.localmd5
			COMMAND md5sum ${basename} > ${basename}.localmd5)

		add_custom_command(
			OUTPUT ${basename}.ok
			COMMAND touch ${basename}.ok
		)

		_get_test_data(${basename}.md5 FROM)
		set(depends ${basename} ${basename}.md5 ${basename}.localmd5 ${basename}.ok)
	endif()

	ADD_CUSTOM_TARGET(${TARGET} DEPENDS ${depends})

endfunction(ecbuild_get_test_data)

ecbuild_get_test_data(msl.grib magics/test/fortran 0)

ecbuild_add_test( TARGET    magics_fortran
	SOURCES   magics.f
	LIBS      MagPlusSingle MagPlus
	LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET magics_fortran_grib
	SOURCES grib.f90 
	LIBS MagPlusSingle MagPlus 
	LINKER_LANGUAGE Fortran)

ADD_DEPENDENCIES(magics_fortran_grib test_data_msl_grib)

