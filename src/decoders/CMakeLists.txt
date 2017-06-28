set( _decoders_srcs
BinningObject.cc
BinningObject.h
BoxPlotDecoder.cc
BoxPlotDecoder.h
Date.cc
DateTime.cc
DateTime.h
EfiLegendEntry.h
EpsXmlInput.cc
EpsXmlInput.h
GeoPointsDecoder.cc
GeoPointsDecoder.h
ImportAction.h
InputData.cc
InputData.h
InputMatrix.cc
InputMatrix.h
InputMatrixInterpretor.cc
InputMatrixInterpretor.h
LandgramDecoder.cc
LandgramDecoder.h
LocalTable.cc
LocalTable.h
MapGenDecoder.cc
MapGenDecoder.h
MatrixTestDecoder.cc
MatrixTestDecoder.h
ObsStatDecoder.cc
ObsStatDecoder.h
ShapeDecoder.cc
ShapeDecoder.h
SimplePolylineInput.cc
SimplePolylineInput.h
SymbolInput.h
TableDecoder.cc
TableDecoder.h
ThinningMethod.cc
ThinningMethod.h
Time.cc
TitleField.cc
TitleField.h
TitleMetaField.cc
TitleMetaField.h
TitleStringField.cc
TitleStringField.h
TitleTemplate.cc
TitleTemplate.h
WindMode.cc
WindMode.h
XYList.cc
XYList.h
shapefil.h
shpopen.c
dbfopen.c
safileio.c
)

foreach( file ${_decoders_srcs} )
  list( APPEND decoders_srcs decoders/${file} )
endforeach()

if (HAVE_GRIB)
     list( APPEND decoders_srcs 
decoders/GribAddressMode.h
decoders/GribDecoder.cc
decoders/GribDecoder.h
decoders/GribInterpretor.h
decoders/GribLoopStep.cc
decoders/GribLoopStep.h
decoders/GribRegularInterpretor.cc
decoders/GribRegularInterpretor.h
decoders/GribSatelliteInterpretor.cc
decoders/GribSatelliteInterpretor.h
)
endif()


if (HAVE_NETCDF) 
     list( APPEND decoders_srcs 
#Netcdf--->
decoders/NetcdfOrcaInterpretor.cc
decoders/NetcdfData.cc
decoders/NetcdfData.h
decoders/NetcdfConvention.cc
decoders/NetcdfConvention.h
decoders/NetcdfDecoder.cc
decoders/NetcdfDecoder.h
decoders/NetcdfGeoMatrixInterpretor.cc
decoders/NetcdfGeoMatrixInterpretor.h
decoders/NetcdfGeopointsInterpretor.cc
decoders/NetcdfGeopointsInterpretor.h
decoders/NetcdfInterpretor.cc
decoders/NetcdfInterpretor.h
decoders/NetcdfMarsConvention.cc
decoders/NetcdfMarsConvention.h
decoders/NetcdfMatrixInterpretor.cc
decoders/NetcdfMatrixInterpretor.h
decoders/NetcdfOrcaInterpretor.h
decoders/NetcdfVectorInterpretor.cc
decoders/NetcdfVectorInterpretor.h
#<----Netcdf
)
endif()

if (HAVE_BUFR)
     list( APPEND decoders_srcs 
# Bufr --->
decoders/EpsBufr.cc
decoders/EpsBufr.h
decoders/ObsDecoder.cc
decoders/ObsDecoder.h
# <--- Bufr
)

endif()


if (HAVE_SPOT)
     list( APPEND decoders_srcs 
#Spot ---> 
decoders/ClassicMtgDecoder.cc
decoders/ClassicMtgDecoder.h
decoders/EpsgramDecoder.cc
decoders/EpsgramDecoder.h
#<--- Spot  
)
endif()

set( decoders_srcs ${decoders_srcs} PARENT_SCOPE )

if ( metview )
	list (APPEND metview_include 
			decoders/DateTime.h 
			decoders/GribDecoder.h 
			decoders/ThinningMethod.h 
			decoders/BinningObject.h  
			decoders/WindMode.h  
			decoders/GribAddressMode.h  
			decoders/GribLoopStep.h  
			decoders/GribInterpretor.h
			decoders/GribSatelliteInterpretor.h
			
			)
	set( metview_include ${metview_include} PARENT_SCOPE )
endif()

