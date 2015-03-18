list( APPEND _drivers_srcs

# basic

  BaseDriver.h BinaryDriver.h PostScriptDriver.h DriverManager.h
  System.h BaseDriverImages.h BaseDriverWind.h BaseDriverSymbols.h 
  BaseDriver.cc DriverManager.cc BinaryDriver.cc PostScriptDriver.cc

# minizip
  minizip/ioapi.c minizip/zip.c
  minizip/ioapi.h minizip/zip.h minizip/crypt.h

# libimagequant
  libimagequant/blur.h libimagequant/libimagequant.c libimagequant/mediancut.c	
  libimagequant/mempool.c libimagequant/nearest.c libimagequant/pam.c 
  libimagequant/viter.c libimagequant/blur.c libimagequant/libimagequant.h	
  libimagequant/mediancut.h	libimagequant/mempool.h	libimagequant/nearest.h	libimagequant/pam.h	libimagequant/viter.h
  libimagequant/rwpng.c libimagequant/rwpng.h
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
          MgQ/MgQPlotScene.cc
		  MgQ/MgQLayoutItem.cc MgQ/MgQStepItem.cc
		  MgQ/MgQLayerItem.cc
		  MgQ/MgQSymbol.cc MgQ/MgQPixmapItem.cc MgQ/MgQPathItem.cc
		  MgQ/MgQTextItem.cc
		  MgQ/MgQPolylineSetItem.cc MgQ/MgQScene.cc MgQ/MgQSceneItem.cc
		  MgQ/MgQSceneCacheItem.cc MgQ/MgQStepMetaData.cc
          MgQ/MgQPattern.cc MgQ/MgQHistoItem.cc
      )
      
   
   
   set (qt_files_HEADERS MgQ/MgQPlotScene.h)
   QT4_WRAP_CPP(qt_files_HEADERS_MOC ${qt_files_HEADERS})
  
   list( APPEND qt_srcs   ${qt_files_HEADERS_MOC}) 

endif()

if( MAGICS_CAIRO )
  list( APPEND _drivers_srcs CairoDriver.h CairoDriver.cc )
endif()

if ( ENABLE_METVIEW )
	list (APPEND metview_include 
				drivers/DriverManager.h
				drivers/BaseDriver.h 
				${CMAKE_CURRENT_BINARY_DIR}/../params/BaseDriverAttributes.h
				drivers/MgQ/MgQPlotScene.h
				drivers/MgQ/MgQScene.h
				drivers/QtDriver.h 
				drivers/MgQ/MgQLayoutItem.h
				drivers/MgQ/MgQ.h
				drivers/MgQ/MgQSceneItem.h
				drivers/MgQ/MgQLayerItem.h
				drivers/MgQ/MgQRootItem.h
				drivers/MgQ/MgQStepMetaData.h
		)
	set( metview_include ${metview_include} PARENT_SCOPE )
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

