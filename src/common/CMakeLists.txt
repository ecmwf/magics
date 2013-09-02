list( APPEND _common_srcs

	OutputFactory.h MagLog.h
	BaseParameter.h Colour.h MagException.h
	Factory.cc Factory.h
	ParameterManager.h Frame.h Layout.h
	PaperPoint.h GeoPoint.h  Tokenizer.h Node.h MagTranslator.h
	ImageProperties.h LogoPlotting.h
	ImportObject.h ImportObject.cc ColourTable.h PageID.h
	Transformation.h Dimension.h
	XmlNode.h XmlTree.h XmlReader.h BackgroundColour.h
	ColourTableDefinition.h ColourTableDefinitionCompute.h ColourTableDefinitionComputeInterface.h
	ColourTableDefinitionList.h ColourTableDefinitionListInterface.h 
	TableDefinition.h TableDefinitionList.h TableDefinitionListInterface.h
	TableDefinitionCompute.h TableDefinitionComputeInterface.h
	HeightTableDefinition.h LevelTableDefinition.h IntervalMap.h PaperDimension.h
	Matrix.h MatrixHandler.h Data.h Decoder.h
	BasePointsHandler.h RasterData.h  Visualiser.h CustomisedPoint.h PointsHandler.h
	MetaData.h
	Polyline.h
	Text.h
	Symbol.h
	BinaryObject.h
	Arrow.h
	Flag.h Flag.cc
	Image.h 
	ArrowProperties.h
	MagicsParameter.cc MagicsParameter.h Clipper.h LogoPlottingBase.h
	TableDefinitionCompute.cc 
	TableDefinitionList.cc
	Coordinate.h OutputHandler.h
	MagFont.h 
	AnimationRules.h SymbolItem.h
	UserPoint.h Label.h
	Timer.h
	CustomisedPoint.h Static.h DriverStatic.h ProgressObject.h PolyCoast.h
	ProgressTask.h FullPaperPoint.h
	BasePointsHandler.h PointsHandler.h Decoder.h Data.h
	Visualiser.h MatrixHandler.h
	Seconds.h
	Matrix.h Matrix.cc
	Path.h
	MagicsSingleton.h
	PlotManager.h
	Image.h 
	XmlReader.h XmlTree.h XmlNode.h
	MagicsFormat.h
	CartesianTransformation.h
	Dimension.cc
	XCoordinate.h YCoordinate.h marsmachine.h linux.h aix.h
	Thread.h MutexCond.h Mutex.h AutoLock.h ThreadControler.h Task.h MagExceptions.h ThreadSingleton.h ThreadSingleton.cc
	DataStats.h SciMethods.h

# sources
	Data.cc Matrix.cc MagicsCalls.cc Static.cc DriverStatic.cc MagLog.cc Colour.cc Transformation.cc MagException.cc
	BaseParameter.cc ParameterManager.cc  
	Tokenizer.cc Timer.cc Seconds.cc Label.cc
	ArrowProperties.cc
	XmlReader.cc XmlTree.cc XmlNode.cc Dimension.cc
	MetaData.cc
	MagicsFormat.cc
	OutputHandler.cc  OutputFactory.cc
	MagFont.cc
	ColourTable.cc ColourTableDefinitionList.cc
	ColourTableDefinitionCompute.cc CartesianTransformation.cc
	BinaryObject.cc
	Coordinate.cc
	LogoPlotting.cc
	PageID.cc AnimationRules.cc Layout.cc SymbolItem.cc Symbol.cc Polyline.cc
	Text.cc Flag.cc  
	Mutex.cc MutexCond.cc Thread.cc MagExceptions.cc AutoLocker.cc
	ThreadControler.cc Task.cc
	DataStats.cc SciMethods.cc BasePointsHandler.cc PointsHandler.cc

# projection
    GeoRectangularProjection.cc  PolarStereographicProjection.cc TaylorProjection.cc
    GeoRectangularProjection.h   PolarStereographicProjection.h SatelliteProjection.h TaylorProjection.h
    Proj4Projection.cc  Proj4Projection.h

# unused

# FrameBase.cc 
# FrameBase.h
# AutomaticPlotManager.cc
# AutomaticPlotManager.h
# FrameBase.cc
# FrameBase.h
# FrameLoop.cc
# FrameLoop.h

 )


list( APPEND _common_templates 
    Factory.cc 
    MagicsParameter.cc
    TableDefinitionList.cc
    TableDefinitionCompute.cc
 )

foreach( file ${_common_srcs} )
  list( APPEND common_srcs common/${file} )
endforeach()
foreach( file ${_common_templates} )
  list( APPEND common_templates common/${file} )
endforeach()

set( common_srcs ${common_srcs} PARENT_SCOPE )
set( common_templates ${common_templates} PARENT_SCOPE )

    
