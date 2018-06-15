list( APPEND _common_xmls
Akima474Method.xml
Akima760Method.xml
ArrowPlotting.xml
AutomaticContourMethod.xml
Axis.xml
AxisMethod.xml
Bar.xml
BaseDriver.xml
BinaryDriver.xml
BinaryObject.xml
BinningObject.xml
BothValuePlotMethod.xml
Boundaries.xml
CalculateColourTechnique.xml
GradientsColourTechnique.xml
PaletteColourTechnique.xml
CalculateHeightTechnique.xml
CalmIndicator.xml
CartesianTransformation.xml
CellShading.xml
CircleOriginMarker.xml
Cities.xml
ClassicMtgDecoder.xml
CoastPlotting.xml
Coastlines.xml
ColourTechnique.xml
ContinuousLegendMethod.xml
Contour.xml
ContourMethod.xml
CountSelectionType.xml
Curve.xml
CurveArea.xml
DateAxisMethod.xml
DateGribLoopStep.xml
DotGraphShadeStyle.xml
DotOriginMarker.xml
DotPolyShadingMethod.xml
DumpShading.xml
Emagram.xml
EmagramGrid.xml
FlagPlotting.xml
FortranRootSceneNode.xml
FortranSceneNode.xml
FortranViewNode.xml
GeoPointsDecoder.xml
GeoRectangularProjection.xml
Graph.xml
GraphArrow.xml
GraphFlag.xml
GraphPlotting.xml
GraphShade.xml
GraphShadeStyle.xml
GridPlotting.xml
GridShading.xml
HatchGraphShadeStyle.xml
HatchPolyShadingMethod.xml
HeightTechnique.xml
HiLo.xml
HiLoBase.xml
HiLoBoth.xml
HiLoMarker.xml
HiLoMarkerBase.xml
HiLoNumber.xml
HiLoTechnique.xml
HiLoText.xml
HighHiLo.xml
Histogram.xml
HistogramLegendMethod.xml
HyperAxisMethod.xml
ImportAction.xml
ImportPlot.xml
InputData.xml
LogoPlotting.xml
NoLogoPlotting.xml
IntervalSelectionType.xml
IsoHighlight.xml
IsoLabel.xml
IsoPlot.xml
IsoShading.xml
KMLDriver.xml
GeoJsonDriver.xml
LabelPlotting.xml
ObsPlotting.xml
LegendMethod.xml
LegendVisitor.xml
LevelListSelectionType.xml
LevelSelection.xml
ListColourTechnique.xml
ListHeightTechnique.xml
LogarithmicAxisMethod.xml
LookupTableMode.xml
LowHiLo.xml
MagnifierVisitor.xml
MapGenDecoder.xml
MarkerShadingTechnique.xml
MarkerValuePlotMethod.xml
MercatorProjection.xml
MetaData.xml
MvRootSceneNode.xml
NoBinningObject.xml
NoBoundaries.xml
NoCalmIndicator.xml
NoCities.xml
NoCoastPlotting.xml
NoGraphShade.xml
NoGridPlotting.xml
NoHiLo.xml
NoHiLoMarker.xml
NoIsoHighlight.xml
NoIsoLabel.xml
NoIsoPlot.xml
NoIsoShading.xml
NoLabelPlotting.xml
#NoLegendVisitor.xml
NoLogoPlotting.xml
NoMagnifierVisitor.xml
NoOriginMarker.xml
NoOutLayerTechnique.xml
NoPageID.xml
NoPreviewVisitor.xml
NoValuePlot.xml
OriginMarker.xml
OutputHandler.xml
PageID.xml
ParamGribLoopStep.xml
PolarStereographicProjection.xml
PolyShadingMethod.xml
PolyShadingTechnique.xml
PositionListAxisMethod.xml
PostScriptDriver.xml
PreviewVisitor.xml
Proj4Bonne.xml
Proj4Mercator.xml
Proj4Collignon.xml
Proj4EPSG32661.xml
Proj4EPSG4326.xml
Proj4EPSG3857.xml
Proj4EPSG900913.xml
Proj4Geos.xml
Proj4Goode.xml
Proj4Lambert.xml
Proj4Mollweide.xml
Proj4PolarNorth.xml
Proj4Projection.xml
Proj4Robinson.xml
Proj4Google.xml
Proj4Efas.xml
Proj4TPers.xml
Proj4LambertNorthAtlantic.xml
Proj4Meteosat0.xml
Proj4Meteosat145.xml
Proj4Meteosat57.xml
Proj4Geosw.xml
Proj4Geose.xml
SDWindMode.xml
SVGDriver.xml
ImportObjectHandler.xml
SelectionMode.xml
ShadingTechnique.xml
ShapeDecoder.xml
SimpleOutLayerTechnique.xml
Skewt.xml
SkewtGrid.xml
SymbolAdvancedTableMode.xml
SymbolIndividualMode.xml
SymbolInput.xml
SymbolMode.xml
SymbolPlotting.xml
SymbolTableMode.xml
Streamlines.xml
TableDecoder.xml
TaylorGrid.xml
TaylorProjection.xml
TextVisitor.xml
Tephigram.xml
TephiGrid.xml
TitleTemplate.xml
Transformation.xml
UVWindMode.xml
UserLogoPlotting.xml
VDWindMode.xml
ValuePlot.xml
ValuePlotBase.xml
ValuePlotMethod.xml
Wind.xml
WindMode.xml
WindPlotting.xml
ObsDecoder.xml
XCoordinate.xml
XDateCoordinate.xml
XHyperCoordinate.xml
XLogarithmicCoordinate.xml
XRegularCoordinate.xml
XYList.xml
GeoJSon.xml
YCoordinate.xml
YDateCoordinate.xml
YHyperCoordinate.xml
YLogarithmicCoordinate.xml
YRegularCoordinate.xml
)

list( APPEND _magics_xmls BoxPlotBox.xml
		BoxPlotBoxBorder.xml
		BoxPlotDecoder.xml
		BoxPlotMedian.xml
		BoxPlotVisualiser.xml
		BoxPlotWhiskerBorder.xml
		BoxPlotWhiskerBox.xml
		BoxPlotWhiskerLine.xml
		CdfGraph.xml
		EpsCloud.xml
		EpsDirection.xml
		EpsGraph.xml
		EpsPlume.xml
		EpsShade.xml
		EpsWave.xml
		EpsWind.xml
		CapeBox.xml
		EpsXmlInput.xml
		EpsgramDecoder.xml
		EquidistantTableMode.xml
		FixedTableMode.xml
		EfiDataDecoder.xml
		EfiGraph.xml
		EfiJSon.xml
		EfigramDecoder.xml
		EpsBufr.xml
		ImageCalculateColourTechnique.xml
		ImageListColourTechnique.xml
		#ImagePlotting.xml
		InputMatrix.xml
		InputMatrixIrregularInterpretor.xml
		InputMatrixRegularInterpretor.xml
		LandgramBox.xml
		LandgramDecoder.xml
		MetgramCurve.xml
		MetgramFlags.xml
		MetgramGraph.xml
		MetgramBar.xml
		ObsTable.xml
		ObsJSon.xml
		ObsStatDecoder.xml
		SimplePolyline.xml
		SimplePolylineInput.xml
		WrepJSon.xml
		WrepRootNode.xml
		XmlBasicNode.xml
		XmlRootNode.xml
		XmlSceneNode.xml
		XmlViewNode.xml
)


if( HAVE_GRIB )
	list( APPEND _common_xmls GribAddressByteMode.xml
		GribAddressMode.xml
		GribAddressRecordMode.xml
		GribLoop.xml
		GribLoopStep.xml
		GribDecoder.xml
		)
endif()

if( HAVE_NETCDF )
	list( APPEND _common_xmls
		NetcdfDecoder.xml
		NetcdfInterpretor.xml
		NetcdfXYpointsInterpretor.xml
		NetcdfOrcaInterpretor.xml
		NetcdfGeopointsInterpretor.xml
		NetcdfGeoPolarMatrixInterpretor.xml
		NetcdfGeoVectorInterpretor.xml
		NetcdfGuessInterpretor.xml
		NetcdfVectorInterpretor.xml
		NetcdfGeoMatrixInterpretor.xml
		NetcdfMatrixInterpretor.xml
		)
endif()

if ( HAVE_ODB )
	list ( APPEND _common_xmls
		OdaGeoDecoder.xml
		OdaXYDecoder.xml )
endif()

if( HAVE_CAIRO )
    list( APPEND _common_xmls CairoDriver.xml )
endif()

if( MAGICS_RASTER )
  list( APPEND _common_xmls GDDriver.xml )
endif()

if( MAGICS_QT )
  list( APPEND  _magics_xmls QtDriver.xml )
endif()

foreach( file ${_common_xmls} )
  list( APPEND metview_xmls params/${file} )
  list( APPEND magics_xmls params/${file} )
endforeach()

foreach( file ${_magics_xmls} )
  list( APPEND magics_xmls params/${file} )
endforeach()


set( magics_xmls ${magics_xmls} PARENT_SCOPE )

if( metview )
  set( metview_xmls ${metview_xmls} PARENT_SCOPE )
  add_metview_definition_file(FILE ImportObjectHandler.xml  BASENAME MIMPORT   ACTION MIMPORT)
  add_metview_definition_file(FILE Contour.xml         BASENAME MCONT   ACTION MCONT)
  add_metview_definition_file(FILE SymbolPlotting.xml  BASENAME MSYMB ACTION   MSYMB)
  add_metview_definition_file(FILE Coastlines.xml      BASENAME MCOAST  ACTION MCOAST)
  add_metview_definition_file(FILE LegendVisitor.xml   BASENAME MLEGEND ACTION MLEGEND)
  add_metview_definition_file(FILE Wind.xml            BASENAME MWIND   ACTION MWIND)
  add_metview_definition_file(FILE GraphPlotting.xml   BASENAME MGRAPH  ACTION MGRAPH)
  add_metview_definition_file(FILE TextVisitor.xml     BASENAME MTEXT   ACTION MTEXT)
  add_metview_definition_file(FILE Axis.xml            BASENAME MAXIS   ACTION MAXIS)
  add_metview_definition_file(FILE BinningObject.xml   BASENAME BinningObject  ACTION MBINNING)
  add_metview_definition_file(FILE TephiGrid.xml       BASENAME MTHERMOGRID  ACTION MTHERMOGRID)
  add_metview_definition_file(FILE ObsPlotting.xml     BASENAME ObsPlotting  ACTION MOBS)
endif( metview )
