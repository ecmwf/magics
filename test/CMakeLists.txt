
return()
if ( ENABLE_FORTRAN )
	ecbuild_enable_fortran( REQUIRED )
	add_subdirectory( fortran )
endif()

if ( ENABLE_PYTHON )
	add_subdirectory( python )
endif()

add_subdirectory( C )
add_subdirectory( magml )
