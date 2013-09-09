

set( _odb_srcs
	OdaDecoder.h
	ODACalls.cc
	OdaDecoder.cc
)

foreach( file ${_odb_srcs} )
	list( APPEND metview_srcs oda/${file} )
endforeach()

set( odb_srcs ${odb_srcs} PARENT_SCOPE )


