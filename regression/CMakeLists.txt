
if( NOT ENABLE_REGRESSION ) 
	return()
endif()

set( REGRESSION_SCRIPT_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR} )

#set tests timestamp
execute_process(COMMAND date +\"%Y%m%d_%H%M%S\" OUTPUT_VARIABLE TIMESTAMP)
STRING(REGEX REPLACE "\"" "" TIMESTAMP ${TIMESTAMP})

STRING(REGEX REPLACE "\n" ""  TIMESTAMP ${TIMESTAMP})

#set( summary_depend "")

function( add_magics_test )

    set( options PYTHON  JSON)
    set( single_value_args NAME IMAGE THRESHOLD)
    set( multi_value_args  SOURCES DATA RESOURCES )
    cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_p_UNPARSED_ARGUMENTS)
      message(FATAL_ERROR "Unknown keywords given to add_magics_test(): \"${_p_UNPARSED_ARGUMENTS}\"")
    endif()
    
    ### check parameters

    if( NOT _p_NAME )
      message(FATAL_ERROR "add_magics_test() expects a NAME")
    endif()

    if( NOT _p_IMAGE )
      message(FATAL_ERROR "add_magics_test() expects a IMAGE")
    endif()
    if( NOT _p_THRESHOLD )
      set(_p_THRESHOLD "20")
    endif()
    
    list(APPEND tools "report_template.html")
    list(APPEND tools "summary_template.html")    
    
   	foreach( file ${tools} )
          execute_process( COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/../${file} ${CMAKE_CURRENT_BINARY_DIR} )
    endforeach()
    
	get_filename_component(where ${CMAKE_CURRENT_SOURCE_DIR} NAME )
    set( targets "" )
	foreach( data ${_p_DATA} )
        string( REGEX REPLACE "[^A-Za-z0-9_]" "_" target "${_p_NAME}_data_${data}" )
        ecbuild_get_test_data( NAME ${data} TARGET ${target} NOCHECK 
            DIRNAME magics/regression/${where})
        list( APPEND targets  "${target}")
	endforeach()

	set( versions "" ) 


	get_filename_component(log ${_p_IMAGE} NAME_WE)
             
	
	foreach( version ${MAGICS_REFERENCE_VERSIONS} )
	    
		set( file ${version}_${_p_IMAGE} )	 
		set( err ${version}_${log}.err )
		set( out ${version}_${log}.out )	
		set( usa ${version}_${log}.usa )
		set (todos)  
		list (APPEND todos ${file})
		list (APPEND todos ${err})
		list (APPEND todos ${out})
		list (APPEND todos ${usa})		
		foreach ( todo  ${todos} )
        	string( REGEX REPLACE "[^A-Za-z0-9_]" "_" target "test_data_${todo}" )
        	list( APPEND targets "${target}")
	    
			ecbuild_get_test_data( NAME ${todo} NOCHECK 
				DIRNAME magics/reference/${version}/${where} )
		endforeach()
	
		set( versions "${versions}${version} " )
	 
	endforeach()

	
	set(target "magics_a_${_p_NAME}")

	#summary depends of all previous tests
    #list(APPEND summary_depend "${target}")
	
	set(target_load "magics_load_${_p_NAME}")
	
	list(APPEND _p_RESOURCES ${_p_SOURCES} )

    if( _p_PYTHON )
	    
		 if ( ENABLE_REGRESSION_UPLOAD ) 
			ecbuild_add_test(  TARGET   ${target_load}
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/upload.py
                   DEPENDS  ${targets}                     
                   RESOURCES  ${_p_RESOURCES} 
                   ARGS ${MAGICS_VERSION} ${_p_SOURCES} ${_p_IMAGE} magics/reference/${MAGICS_VERSION}/${where} -i python
                   ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
         endif()
         
         ecbuild_add_test(  TARGET   ${target}
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   DEPENDS  ${targets}  
                   RESOURCES  ${_p_RESOURCES} 
                   ARGS ${TIMESTAMP} ${_p_SOURCES} ${_p_IMAGE} -n ${BRANCH_NAME} -t ${_p_THRESHOLD}  -o ${MAGICS_HTML_ROOT}  -v ${versions} -f -i python
            ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
        
            return() 
    endif()
	if( _p_JSON )
	       if ( ENABLE_REGRESSION_UPLOAD ) 
				ecbuild_add_test(  TARGET   ${target_load}
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/upload.py
                   DEPENDS  ${targets} 
                   
                   RESOURCES  ${_p_RESOURCES} 
                   ARGS ${MAGICS_VERSION} ${_p_SOURCES} ${_p_IMAGE} magics/reference/${MAGICS_VERSION}/${where} -i ${CMAKE_BINARY_DIR}/bin/magjsonx
				 )
            endif()     
			ecbuild_add_test(  TARGET   ${target}
                   TYPE SCRIPT
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py
                   DEPENDS  ${targets}  
                   RESOURCES  ${_p_RESOURCES} 
                   ARGS ${TIMESTAMP} ${_p_SOURCES} ${_p_IMAGE}  -n ${BRANCH_NAME} -t ${_p_THRESHOLD}  -o ${MAGICS_HTML_ROOT}  -v ${versions} -i ${CMAKE_BINARY_DIR}/bin/magjsonx -f
					ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
            return()
	endif()
# Run Test
		if ( ENABLE_REGRESSION_UPLOAD ) 
           
			ecbuild_add_test(  TARGET   ${target_load}
                   TYPE     SCRIPT
                   SOURCES  ${_p_SOURCES}                    
                   LIBS MagPlusShared MagPlusSingleShared ${MAGICS_EXTRA_LIBRARIES} 
                   DEPENDS ${targets}
                   RESOURCES  ${_p_RESOURCES} 
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/upload.py  
                   ARGS  ${MAGICS_VERSION} ./${target} ${_p_IMAGE} magics/reference/${MAGICS_VERSION}/${where} ) 
		endif()
	    ecbuild_add_test(  TARGET   ${target}
                   TYPE     SCRIPT
                   SOURCES  ${_p_SOURCES}                    
                   LIBS MagPlusShared MagPlusSingleShared ${MAGICS_EXTRA_LIBRARIES} 
                   DEPENDS ${targets}
                   RESOURCES  ${_p_RESOURCES} 
                   COMMAND  ${REGRESSION_SCRIPT_DIRECTORY}/compare.py  
                   ARGS  ${TIMESTAMP} ./${target} ${_p_IMAGE}  -n ${BRANCH_NAME}  -t ${_p_THRESHOLD}  -o ${MAGICS_HTML_ROOT}  -v ${versions} -f )


endfunction(add_magics_test)




add_subdirectory( basic )
add_subdirectory( efas )
add_subdirectory( macc )
add_subdirectory( plumes )
add_subdirectory( axis )
add_subdirectory( contour )
add_subdirectory( web )
add_subdirectory( data )
add_subdirectory( metops )
add_subdirectory( wind )
add_subdirectory( graph )
add_subdirectory( layout )
add_subdirectory( rotated )
add_subdirectory( proj4 )
add_subdirectory( venice )
add_subdirectory( gallery )
add_subdirectory( era )
add_subdirectory( symbol )


#final test 
ecbuild_add_test(  TARGET   magics_summary
                   TYPE PYTHON
                   DEPENDS ${summary_depend}
                   RESOURCES summary_template.html
                   COMMAND   ${CMAKE_CURRENT_SOURCE_DIR}/summary.py
                   ARGS ${MAGICS_HTML_ROOT} 
                   )                   
