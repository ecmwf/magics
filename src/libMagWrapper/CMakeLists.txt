set( _metview_srcs
MagPlus.h
MagRequest.h
MagPlus.cc
)

foreach( file ${_metview_srcs} )
  list( APPEND metview_srcs libMagWrapper/${file} )
endforeach()



set( metview_srcs ${metview_srcs} PARENT_SCOPE )
