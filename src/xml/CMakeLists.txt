list( APPEND ${xml2cc_srcs}
    Proj4Projection.xml Drivers.xml  output.xml BaseDriver.xml PostScriptDriver.xml SVGDriver.xml BinaryDriver.xml KMLDriver.xml
    Akima.xml ActionRoutines.xml HiLoText.xml XYList.xml
    BoxPlot.xml  Histogram.xml   LevelSelection.xml
    Epsgram.xml  Landgram.xml    ImagePlotting.xml      MetaData.xml  SubPage.xml
    SubPageProj4.xml XYTransformation.xml
    Frame.xml              Import.xml        TextVisitor.xml         NetcdfDecoder.xml  SuperPage.xml XmlBox.xml
    Axis.xml               NetcdfInterpretor.xml SymbolInput.xml InputData.xml TableDecoder.xml
    InputMatrix.xml        SymbolPlotting.xml      XmlPage.xml
    Geopoints.xml          Obs.xml        TextVisitor.xml     XmlSubPage.xml
    CoastPlotting.xml      Grib.xml               IsoHighlight.xml   ObsPlotting.xml     graph_params.xml
    Coastlines.xml         GridPlotting.xml       IsoLabel.xml       ObsStat.xml
    HiLo.xml               IsoPlot.xml            OdaDecoder.xml     Title.xml           magml.xml
    Contour.xml            HiLoMarker.xml         IsoShading.xml     TitleTemplate.xml
    HiLoNumber.xml         LabelPlotting.xml      Page.xml           ValuePlot.xml
    Layout.xml         PageID.xml           View.xml
    HiLoTechnique.xml      Legend.xml  Wind.xml   Taylor.xml SelectionMode.xml
    GeoRectangularProjection.xml SatelliteProjection.xml PolarStereographicProjection.xml
    EpsJSon.xml ObsJSon.xml QtDriver.xml CairoDriver.xml GDDriver.xml BinningObject.xml
)

foreach( file ${xml2cc_srcs} )

      # make sure output dir exists
      set( output_dir "" ) 
      file( MAKE_DIRECTORY ${output_dir} )

      get_filename_component( basefile ${file} NAME_WE )

      set( ouputfiles ${basefile}Attributes.h ${basefile}Attributes.cc )

      # generate the code
      add_custom_command(
        OUTPUT  ${output_files}
        COMMAND ${PERL_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tools/xml2cc_new.pl ${file} ${CMAKE_BINARY_DIR}/src
        DEPENDS ${file}
      )

      set_source_files_properties( ${basefile}Attributes.h  GENERATED )
      set_source_files_properties( ${basefile}Attributes.cc GENERATED )

      list( APPEND _xml_srcs ${output_files} )

endforeach()

set( xml_srcs ${_xml_srcs} PARENT_SCOPE )

