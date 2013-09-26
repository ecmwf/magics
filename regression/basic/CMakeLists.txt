# magics test executable run in script

# download data 
ecbuild_get_test_data( NAME data.grib NOCHECK )

set( versions "" ) 

set (targets "test_data_data_grib")

foreach( version ${MAGICS_REFERENCE_VERSIONS} )
	 set( file ${version}_basic.png )	 
	 STRING(REGEX REPLACE "\\." "_" target ${file})
	 list( APPEND targets "test_data_${target}")
	 
     ecbuild_get_test_data( NAME ${file} NOCHECK 
				DIRNAME magics/reference/${version}/basic )
	
	 set( versions "${versions}${version} " )
	 
endforeach()


# Run Test
ecbuild_add_test(  TARGET   magics_a_basic
                   TYPE     SCRIPT
                   SOURCES  basic.c
                   LIBS MagPlus ${MAGICS_EXTRA_LIBRARIES} 
                   DEPENDS ${targets}
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  
                   ARGS  magics_a_basic basic.png -t 20  -h ${MAGICS_HTML_ROOT} -v ${versions})


# contour
ecbuild_get_test_data( NAME z500.grb NOCHECK  )
set(targets "test_data_z500_grb")

foreach( version ${MAGICS_REFERENCE_VERSIONS} )
	 set( file ${version}_contour.ps ) 
	 STRING(REGEX REPLACE "\\." "_" target ${file})
	 list( APPEND targets "test_data_${target}")
     ecbuild_get_test_data( NAME ${file} NOCHECK 
				DIRNAME magics/reference/${version}/basic )

endforeach()

						
# Run Test
ecbuild_add_test(  TARGET   magics_a_contour
                   TYPE     SCRIPT
                   SOURCES  contour.f
                   LINKER_LANGUAGE CXX
                   LIBS MagPlus MagPlusSingle ${MAGICS_EXTRA_LIBRARIES}
                   DEPENDS  ${targets}
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   ARGS  magics_a_contour contour.ps -t 20 -h ${MAGICS_HTML_ROOT} -v ${versions})

## Symbol using python
set(targets "")
foreach( version ${MAGICS_REFERENCE_VERSIONS} )
     set( file ${version}_symbol.png ) 
	 STRING(REGEX REPLACE "\\." "_" target ${file})
	 list( APPEND targets "test_data_${target}")
     ecbuild_get_test_data( NAME ${version}_symbol.png NOCHECK 
				DIRNAME magics/reference/${version}/basic )
	 
endforeach()

# Run Test
ecbuild_add_test(  TARGET  magics_a_symbol
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   DEPENDS  ${targets}
                   RESOURCES symbol.py
                   ARGS symbol.py symbol.png -t 20 -h ${MAGICS_HTML_ROOT} -v ${versions}
                   ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )



