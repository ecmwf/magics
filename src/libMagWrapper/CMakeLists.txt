set( _metview_srcs
MagPlus.h
MagRequest.h
MagPlus.cc
)

foreach( file ${_metview_srcs} )
  list( APPEND metview_srcs libMagWrapper/${file} )
endforeach()

list (APPEND metview_include libMagWrapper/MagPlus.h libMagWrapper/MagRequest.h)
set( metview_include ${metview_include} PARENT_SCOPE )

set( metview_srcs ${metview_srcs} PARENT_SCOPE )
