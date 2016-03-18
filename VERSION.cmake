
set ( metabuilder_version 2.28.1 )
set ( _version 2.28.1 )
if ( MAGICS_BUILD )
    set( ${PROJECT_NAME}_VERSION_STR  "${_version}-${MAGICS_BUILD}" )   
else ()
    set( ${PROJECT_NAME}_VERSION_STR  ${_version})
endif()

set( BRANCH_NAME ${${PROJECT_NAME}_VERSION_STR} )
