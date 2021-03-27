
add_subdirectory( metgram )
if( NOT EC_OS_NAME MATCHES "windows" )
    add_subdirectory( MagMLInterpretor )
endif()

