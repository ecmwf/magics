set( _web_srcs
MagJSon.cc   ObsJSon.cc   WrepJSon.h
MagConfig.cc    MagJSon.h    ObsJSon.h
MagConfig.h     WrepJSon.cc
GeoJSon.h GeoJSon.cc
MagYaml.h MagYaml.cc
 )

foreach( file ${_web_srcs} )
  list( APPEND web_srcs web/${file} )
endforeach()

set( web_srcs ${web_srcs} PARENT_SCOPE )
