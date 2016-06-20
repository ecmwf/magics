set( _terralib_srcs
# headers
     kernel/TeProjection.h kernel/TeProjection.cpp 
     kernel/TeDatum.cpp kernel/TeDatum.h 
     kernel/TeUtils.h kernel/TeUtils.cpp
     kernel/TeGeometry.h kernel/TeGeometry.cpp
     kernel/TeMultiGeometry.h kernel/TeMultiGeometry.cpp
     kernel/TeBox.h kernel/TeBox.cpp
     kernel/TeErrorLog.h kernel/TeErrorLog.cpp
     kernel/TeException.h kernel/TeException.cpp
     kernel/TeCoord2D.h kernel/TeDefines.h kernel/TePrecision.h kernel/TeSingleton.h kernel/TeComposite.h kernel/TeCounted.h kernel/TeMeasure.h kernel/TeDataTypes.h
)
foreach( file ${_terralib_srcs} )
  list( APPEND terralib_srcs terralib/${file} )
endforeach()

set( terralib_srcs ${terralib_srcs} PARENT_SCOPE )
