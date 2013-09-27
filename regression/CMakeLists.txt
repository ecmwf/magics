set( REGRESSION_SCRIPT_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR} )


function( ecbuild_add_magics_test )

    set( options PYTHON )
    set( single_value_args NAME IMAGE THRESHOLD)
    set( multi_value_args  SOURCES DATA RESOURCES )
    cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_p_UNPARSED_ARGUMENTS)
      message(FATAL_ERROR "Unknown keywords given to ecbuild_add_magics_test(): \"${_p_UNPARSED_ARGUMENTS}\"")
    endif()
    

   
    ### check parameters

    if( NOT _p_NAME )
      message(FATAL_ERROR "ecbuild_get_test_data() expects a NAME")
    endif()

    if( NOT _p_IMAGE )
      message(FATAL_ERROR "ecbuild_get_test_data() expects a NAME")
    endif()
    if( NOT _p_THRESHOLD )
      set(_p_THRESHOLD "20")
    endif()
    debug_var(_p_DATA)
    debug_var(_p_NAME)
    
	foreach( data ${_p_DATA} )
	    debug_var(data)   
	    STRING(REGEX REPLACE "\\." "_" target ${data}) 
		ecbuild_get_test_data( NAME ${data} NOCHECK )
		list ( APPEND targets  "test_data_${target}")
	endforeach()

	set( versions "" ) 


	foreach( version ${MAGICS_REFERENCE_VERSIONS} )
		set( file ${version}_${_p_IMAGE} )	 
		STRING(REGEX REPLACE "\\." "_" target ${file})
		list( APPEND targets "test_data_${target}")
	 
		ecbuild_get_test_data( NAME ${file} NOCHECK 
				DIRNAME magics/reference/${version}/basic )
	
		set( versions "${versions}${version} " )
	 
	endforeach()

	set(target "magics_a_${_p_NAME}")
	
	list(APPEND _p_RESOURCES ${_p_SOURCES} )

	if ( _p_PYTHON ) 
	    
		ecbuild_add_test(  TARGET   ${target}
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   DEPENDS  ${targets}  
                   RESOURCES  ${_p_RESOURCES} 
                   ARGS ${_p_SOURCES} ${_p_IMAGE} -t ${_p_THRESHOLD}  -o ${MAGICS_HTML_ROOT}  -v ${versions} 
                   ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
	else()
# Run Test
		ecbuild_add_test(  TARGET   ${target}
                   TYPE     SCRIPT
                   SOURCES  ${_p_SOURCES}                    
                   LIBS MagPlus MagPlusSingle ${MAGICS_EXTRA_LIBRARIES} 
                   DEPENDS ${targets}
                   RESOURCES  ${_p_RESOURCES} 
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  
                   ARGS  ${target} ${_p_IMAGE} -t ${_p_THRESHOLD}  -o ${MAGICS_HTML_ROOT}  -v ${versions} 
		)
	endif()

endfunction(ecbuild_add_magics_test)

add_subdirectory( basic )
