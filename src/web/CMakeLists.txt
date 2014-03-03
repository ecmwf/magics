set( _web_srcs
MagJSon.cc   ObsJSon.cc   WrepJSon.h      json_spirit_reader.h   json_spirit_value.h
MagConfig.cc    MagJSon.h    ObsJSon.h    json_spirit.h           json_spirit_utils.h    json_spirit_writer.cpp
MagConfig.h     WrepJSon.cc  json_spirit_reader.cpp  json_spirit_value.cpp  json_spirit_writer.h json_spirit_writer_options.h

 )

foreach( file ${_web_srcs} )
  list( APPEND web_srcs web/${file} )
endforeach()

set( web_srcs ${web_srcs} PARENT_SCOPE )
