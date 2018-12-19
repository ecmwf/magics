set( _eckit_srcs
	TableReader.h
	TableReader.cc
)



if ( HAVE_BUFR )
	list (APPEND   _eckit_srcs
		MvLocation.cc  
		MvObsSet.cc   
		fmettim.cc  
		fsortabl.cc  
		inc_iostream.h
		MvLocation.h   
		MvObsSet.h 
		fmettim.h
		fsortabl.h
		MvBufr.h 
		MvObs.cc       
		
		fdyntime.cc
		fobject.cc  
		fstatime.cc
		MvBufrObs.h     
		MvObs.h   
		 
		fdyntime.h  
		fobject.h   
		fstatime.h
		MvBufrEdition.cc
		MvBufrEdition.h
		MvBufrElementTable.cc
		MvBufrElementTable.h
	)
endif()

foreach( file ${_eckit_srcs} )
  list( APPEND eckit_srcs eckit_readers/${file} )
endforeach()

set( eckit_srcs ${eckit_srcs} PARENT_SCOPE )
