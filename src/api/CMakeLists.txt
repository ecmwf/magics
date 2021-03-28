list( APPEND _api_srcs
	MagicsCalls.cc
	MagicsCalls.h
	MagicsCallsPython.cc
	MagicsCallsFortran.cc
	MagicsCallsC.cc
	CompatibilityHelper.cc
	CompatibilityHelper.h
 )

install ( FILES magics_api.h  DESTINATION ${INSTALL_INCLUDE_DIR})


foreach( file ${_api_srcs} )
  list( APPEND api_srcs api/${file} )
endforeach()

set( api_srcs ${api_srcs} PARENT_SCOPE )
