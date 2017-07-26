set( _clipper_srcs
# headers
    clipper.hpp clipper.cpp 
)
foreach( file ${_clipper_srcs} )
  list( APPEND clipper_srcs clipper/${file} )
endforeach()

set( clipper_srcs ${clipper_srcs} PARENT_SCOPE )
