list( APPEND _drivers_srcs

# basic

  BaseDriver.h BinaryDriver.h PostScriptDriver.h DriverManager.h
  System.h BaseDriverImages.h BaseDriverWind.h BaseDriverSymbols.h 
  BaseDriver.cc DriverManager.cc BinaryDriver.cc PostScriptDriver.cc

# minizip
  minizip/ioapi.c minizip/zip.c
  minizip/ioapi.h minizip/zip.h minizip/crypt.h

)

if( MAGICS_QT )

  list( APPEND _drivers_srcs
          QtDriver.h
		  MgQ/MgQ.h
          MgQ/MgQLayoutItem.h MgQ/MgQStepItem.h
		  MgQ/MgQLayerItem.h MgQ/MgQPlotScene.h
	      MgQ/MgQSymbol.h MgQ/MgQPixmapItem.h MgQ/MgQPathItem.h
		  MgQ/MgQTextItem.h MgQ/MgQDriverObject.h
		  MgQ/MgQPolylineSetItem.h MgQ/MgQScene.h MgQ/MgQSceneItem.h
		  MgQ/MgQSceneCacheItem.h MgQ/MgQStepMetaData.h
		  MgQ/MgQPattern.h MgQ/MgQHistoItem.h MgQ/MgQRootItem.h
          QtDriver.cc
		  MgQ/MgQLayoutItem.cc MgQ/MgQStepItem.cc
		  MgQ/MgQLayerItem.cc MgQ/MgQPlotScene.cc
		  MgQ/MgQSymbol.cc MgQ/MgQPixmapItem.cc MgQ/MgQPathItem.cc
		  MgQ/MgQTextItem.cc
		  MgQ/MgQPolylineSetItem.cc MgQ/MgQScene.cc MgQ/MgQSceneItem.cc
		  MgQ/MgQSceneCacheItem.cc MgQ/MgQStepMetaData.cc
          MgQ/MgQPattern.cc MgQ/MgQHistoItem.cc
      )

# MgQ/MgQPlotScene.moc.cpp

endif()

if( MAGICS_CAIRO )
  list( APPEND _drivers_srcs CairoDriver.h CairoDriverRaster.cc CairoDriver.cc )
endif()

# if( MAGICS_MING )
#   list( APPEND mgq_srcs MingDriver.h  MingDriver.cc )
# endif()

# KML
list( APPEND _drivers_srcs   KMLDriver.h   KMLDriver.cc )

# SVG
list( APPEND _drivers_srcs   SVGDriver.h   SVGDriver.cc )

if( MAGICS_RASTER )
  list( APPEND mgq_srcs   GDDriver.h  GDDriver.cc )
endif()

# AM_CPPFLAGS	= -DNO_ADDFILEINEXISTINGZIP -I$(top_srcdir)/src -I$(top_srcdir)/src/terralib/kernel -I$(top_srcdir)/src/basic -I$(top_srcdir)/src/drivers -I$(top_srcdir)/src/drivers -I$(top_srcdir)/src/decoders -I$(top_srcdir)/src/visualisers

foreach( file ${_drivers_srcs} )
  list( APPEND drivers_srcs drivers/${file} )
endforeach()

set( drivers_srcs ${drivers_srcs} PARENT_SCOPE )

