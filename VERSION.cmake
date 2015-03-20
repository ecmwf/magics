set( ${PROJECT_NAME}_VERSION_STR  "2.24.2" )
set ( _version 2.24.2 )
if ( MAGICS_BUILD )
    set( ${PROJECT_NAME}_VERSION_STR  "${_version}-${MAGICS_BUILD}" )   
else ()
    set( ${PROJECT_NAME}_VERSION_STR  ${_version})
endif()

set( BRANCH_NAME  ${${PROJECT_NAME}_VERSION_STR} )
