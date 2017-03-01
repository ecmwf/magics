# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


import os




action = {}



action["SymbolPlotting"] = {
    "json" : "symbol",
    "jira" : "Symbol",
    "doc" : "Symbol Plotting documentation",
    "files" : ["SymbolPlotting.xml", "SymbolMode.xml",
        "SymbolIndividualMode.xml",
        "SymbolAdvancedTableMode.xml", "SymbolTableMode.xml",
        "CountSelectionType.xml", "IntervalSelectionType.xml",
        "LevelSelection.xml", "LevelListSelectionType.xml", "CalculateColourTechnique.xml",
        "ColourTechnique.xml", "HeightTechnique.xml", "ListColourTechnique.xml", "CalculateHeightTechnique.xml"]
    }



action["GraphPlotting"] = {
    "json" : "graph",
    "jira" : "Graph Plotting",
    "doc" : "Graph Plotting documentation",
    "files" :["Graph.xml", "GraphPlotting.xml", "Curve.xml", "Bar.xml", "GraphFlag.xml", "GraphArrow.xml",
        "CurveArea.xml", "GraphShade.xml", "NoGraphShade.xml", "GraphShade.xml",
        "NoGraphShade.xml", "GraphShadeStyle.xml", "HatchGraphShadeStyle.xml",
        "DotGraphShadeStyle.xml", "GraphShadeStyle.xml", "HatchGraphShadeStyle.xml",
        "DotGraphShadeStyle.xml", ]
    }

action["Wind"] = {
    "json" : "wind",
    "jira" : "Wind Plotting",
    "doc" : "Wind Plotting documentation",
    "files" : ["Wind.xml", "WindPlotting.xml", "ArrowPlotting.xml", "FlagPlotting.xml", 
            "IntervalSelectionType.xml",
            "CalmIndicator.xml", "NoCalmIndicator.xml","OriginMarker.xml",
            "CalculateColourTechnique.xml", "ColourTechnique.xml",
            "CircleOriginMarker.xml", "DotOriginMarker.xml", "LevelListSelectionType.xml", 
            "CountSelectionType.xml", "LevelSelection.xml"]
    }


action["Contour"] = {
    "json" : "cont",
    "jira" : "Contouring",
    "doc" : "Contouring documentation",
    "files" : ["Contour.xml","IsoPlot.xml", "NoIsoPlot.xml",
    "AutomaticContourMethod.xml", "SampleContourMethod.xml", "ContourMethod.xml",
    "Akima760Method.xml", "Akima474Method.xml", "HiLo.xml", "NoHiLo.xml", "HighHiLo.xml",
    "LowHiLo.xml", "ValuePlot.xml", "NoValuePlot.xml", "IsoHighlight.xml",
    "NoIsoHighlight.xml", "CountSelectionType.xml", "LevelSelection.xml", "IntervalSelectionType.xml",
    "LevelListSelectionType.xml", "IsoLabel.xml", "NoIsoLabel.xml", "IsoShading.xml",
    "NoIsoShading.xml", "HiLoBase.xml", "HiLoTechnique.xml","HiLoText.xml",
    "HiLoNumber.xml", "HiLoBoth.xml", "HiLoMarker.xml","HiLoMarkerBase.xml",
    "NoHiLoMarker.xml", "ValuePlotBase.xml", "ValuePlotMethod.xml", "MarkerValuePlotMethod.xml",
    "BothValuePlotMethod.xml", "ShadingTechnique.xml", "PolyShadingTechnique.xml", "GridShading.xml",
    "CellShading.xml", "DumpShading.xml", "MarkerShadingTechnique.xml", "ColourTechnique.xml",
    "CalculateColourTechnique.xml", "ListColourTechnique.xml", "PolyShadingMethod.xml",
    "DotPolyShadingMethod.xml", "HatchPolyShadingMethod.xml", ]
    }

action["PostScriptDriver"] =  {
    "json" : "postscript",
    "jira" : "Postscript output",
    "doc" : "Postscript Driver documentation",
    "files" : ["PostScriptDriver.xml", "BaseDriver.xml" ]}

action["CairoDriver"] = {
    "json" : "png",
    "jira" : "PNG output",
    "doc" : "CAIRO Driver documentation",
    "files" : ["CairoDriver.xml", "BaseDriver.xml" ] }

action["SVGDriver"] = {
    "json" : "svg",
    "jira" : "SVG output",
    "doc" : "SVG Driver documentation",
    "files" : ["SVGDriver.xml", "BaseDriver.xml" ] }

action["KMLDriver"] = {
    "json" : "kml",
    "jira" : "KML output",
    "doc" : "KML Driver documentation",
    "files" : ["KMLDriver.xml", "BaseDriver.xml" ] }

action["FortranSceneNode"] = {
    "json" : "page",
    "jira" : "Page",
    "doc" : "Page documentation",
    "files" : ["FortranSceneNode.xml", "PageID.xml", "NoPageID.xml", "LogoPlotting.xml", "NoLogoPlotting.xml" ]
    }
action["FortranRootSceneNode"] = {
    "json" :  "superpage",
    "jira" : "Superpage",
    "doc" : "Super Page documentation",
    "files" :  ["FortranRootSceneNode.xml"  ]
    }
action["FortranViewNode"] = {
    "json" : "subpage" ,
    "jira" : "Subpage - Projection",
    "doc" :  "Subpage documentation",
    "files" : ["FortranViewNode.xml", "GeoRectangularProjection.xml", "CartesianTransformation.xml", "Proj4Projection.xml", "PolarStereographicProjection.xml", "Transformation.xml"]
    }

action["Coastlines"] = {
    "json" : "coast" ,
    "jira" : "Coastlines",
    "doc" :  "Coastlines documentation",
    "files" : ["Coastlines.xml", "CoastPlotting.xml", "NoCoastPlotting.xml",
        "LabelPlotting.xml", "NoLabelPlotting.xml",
        "NoGridPlotting.xml", "GridPlotting.xml",
        "Cities.xml",  "NoCities.xml",
        "Boundaries.xml",  "NoBoundaries.xml"]

    }
action["Axis"] = {
    "json" : "axis" ,
    "jira" : "Axis",
    "doc" :  "Axis documentation",
    "files" : ["Axis.xml", "AxisMethod.xml", "LogarithmicAxisMethod.xml",
        "DateAxisMethod.xml", "HyperAxisMethod.xml", "PositionListAxisMethod.xml"]
    }
action["LegendVisitor"] = {
    "json" : "legend" ,
    "jira" : "Legend",
    "doc" :  "Legend documentation",
    "files" :  ["ContinuousLegendMethod.xml",  "HistogramLegendMethod.xml",
              "LegendMethod.xml",  "LegendVisitor.xml",  "NoLegendNode.xml",  "NoLegendVisitor.xml",  "XmlLegendNode.xml"]

    }

action["TextVisitor"] = {
    "json" : "text" ,
    "jira" : "Text Plotting",
    "doc" :  " Text documentation",
    "files" :  ["TextVisitor.xml"]

    }
action["ImportObjectHandler"] = {
    "json" : "import" ,
    "jira" : "Import Object",
    "doc" :  " Import facility documentation",
    "files" :  ["ImportObjectHandler.xml"]

    }

action["GribDecoder"] = {
    "json" : "grib" ,
    "jira" : "Grib Input",
    "doc" :  "Grib Input documentation",
    "files" :  ["GribAddressByteMode.xml", "GribAddressMode.xml", "GribAddressRecordMode.xml",  "GribDecoder.xml",
    "SDWindMode.xml", "UVWindMode.xml", "UVWindMode.xml", "WindMode.xml"] }

action["NetcdfDecoder"] = {
    "json" : "netcdf" ,
    "jira" : "Netcdf Input",
    "doc" :  "Netcdf Input documentation",
    "files" : ["NetcdfDecoder.xml", "NetcdfInterpretor.xml", "NetcdfMatrixInterpretor.xml",
        "NetcdfGeoMatrixInterpretor.xml", "NetcdfVectorInterpretor.xml", "NetcdfGeoVectorInterpretor.xml",
        "NetcdfGeoPolarMatrixInterpretor.xml", "NetcdfGeopointsInterpretor.xml", "NetcdfXYpointsInterpretor.xml", "NetcdfOrcaInterpretor.xml", ]
        }

action["InputData"] = {
    "json" : "input" ,
    "jira" : "Input Data",
    "doc" :  "Input Datadocumentation",
    "files" : ["InputData.xml", "BinningObject.xml"]
        }
action["TableDecoder"] = {
    "json" : "table" ,
    "jira" : "CSV Input",
    "doc" :  "CSV Input Datadocumentation",
    "files" : ["TableDecoder.xml", "BinningObject.xml"]
        }
action["WrepJSon"] = {
    "json" : "wrepjson" ,
    "jira" : "WrepJSon",
    "doc" :  "WrepJSon Datadocumentation",
    "files" : ["WrepJSon.xml"]
        }

'''



action["BoxPlotVisualiser"] = ["BoxPlotBox.xml",  "BoxPlotBoxBorder.xml",  "BoxPlotDecoder.xml",
        "BoxPlotMedian.xml",  "BoxPlotVisualiser.xml",  "BoxPlotWhiskerBorder.xml",
        "BoxPlotWhiskerBox.xml", "BoxPlotWhiskerLine.xml", "NoBoxPlotBoxBorder.xml",  "NoBoxPlotMedian.xml"]

createAction("boxplot",  "BoxPlotVisualiser", action["BoxPlotVisualiser"])




createAction("taylor",  "TaylorGrid", ["Taylor.xml"])



createAction("tephi",  "TephiGrid", ["Tephigram.xml"])

createAction("wrepjson",  "WrepJSon", ["EpsJSon.xml"])

createAction("geo_odb",  "OdaGeoDecoder", ["OdaDecoder.xml"])
createAction("xy_odb",  "OdaXYDecoder", ["OdaDecoder.xml"])




action["BoxPlotVisualiser"] = ["BoxPlotBox.xml",  "BoxPlotBoxBorder.xml",  "BoxPlotDecoder.xml",
        "BoxPlotMedian.xml",  "BoxPlotVisualiser.xml",  "BoxPlotWhiskerBorder.xml",
        "BoxPlotWhiskerBox.xml", "BoxPlotWhiskerLine.xml", "NoBoxPlotBoxBorder.xml",  "NoBoxPlotMedian.xml"]




createAction("postscript",  "PostScript", action["PostScript"])
createAction("boxplot",  "BoxPlotVisualiser", action["BoxPlotVisualiser"])



createAction("taylor",  "TaylorGrid", ["Taylor.xml"])
createAction("Netcdf",  "Wind", ["Wind.xml"])
createAction("text",  "TextVisitor", ["TextVisitor.xml"])



createAction("legend",  "LegendVisitor", ["Legend.xml"])
createAction("tephi",  "TephiGrid", ["Tephigram.xml"])

createAction("wrepjson",  "WrepJSon", ["EpsJSon.xml"])
createAction("input",  "InputData", ["InputData.xml"])
createAction("geo_odb",  "OdaGeoDecoder", ["OdaDecoder.xml"])
createAction("xy_odb",  "OdaXYDecoder", ["OdaDecoder.xml"])


action["NetcdfDecoder"] = ["NetcdfDecoder.xml", "NetcdfInterpretor.xml", "NetcdfMatrixInterpretor.xml",
        "NetcdfGeoMatrixInterpretor.xml", "NetcdfVectorInterpretor.xml", "NetcdfGeoVectorInterpretor.xml",
        "NetcdfGeoPolarMatrixInterpretor.xml", "NetcdfGeopointsInterpretor.xml", "NetcdfXYpointsInterpretor.xml", "NetcdfOrcaInterpretor.xml", ]


action["BoxPlotVisualiser"] = ["BoxPlotBox.xml",  "BoxPlotBoxBorder.xml",  "BoxPlotDecoder.xml",
        "BoxPlotMedian.xml",  "BoxPlotVisualiser.xml",  "BoxPlotWhiskerBorder.xml",
        "BoxPlotWhiskerBox.xml", "BoxPlotWhiskerLine.xml", "NoBoxPlotBoxBorder.xml",  "NoBoxPlotMedian.xml"]




createAction("postscript",  "PostScript", action["PostScript"])
createAction("boxplot",  "BoxPlotVisualiser", action["BoxPlotVisualiser"])



createAction("taylor",  "TaylorGrid", ["Taylor.xml"])

createAction("text",  "TextVisitor", ["TextVisitor.xml"])
createAction("coast", "Coastlines",  ["Coastlines.xml", "CoastPlotting.xml", "LabelPlotting.xml", "GridPlotting.xml"])
createAction("axis",  "Axis", ["Axis.xml"])



createAction("tephi",  "TephiGrid", ["Tephigram.xml"])
createAction("input",  "InputData", ["InputData.xml", "BinningObject.xml"])
createAction("wrepjson",  "WrepJSon", ["EpsJSon.xml"])

createAction("geo_odb",  "OdaGeoDecoder", ["OdaDecoder.xml"])
createAction("xy_odb",  "OdaXYDecoder", ["OdaDecoder.xml"])
'''

def put(file, where, comment):
    cmd = "java -jar /home/graphics/cgs/atlassian-cli-2.5.0/lib/confluence-cli-2.5.0.jar --server http://ussoftware.ecmwf.int:8081/wiki --user cgs --password sjnclEC4 --action addAttachment --space MAGP --file '%s' --comment '%s' --title '%s' " % (file, comment, where)
    print cmd
    os.system(cmd)
"""
list = [

    ["legend.json",    "Legend", "Legend documentation"],
    ["input.json",    "Input Data", "Input Data  documentation"],
    ["wrepjson.json", "WrepJSon", "WrepJSon documentation"],


    ["axis.json",     "Axis", "Axis documentation"],


    ["magdoc.js",     "Reference guide", "Magics gallery generator"],




]
"""



all = [ "Contour", "GraphPlotting", "Wind",
    "PostScriptDriver", "CairoDriver", "KMLDriver", "SVGDriver",
    "FortranRootSceneNode",  "FortranSceneNode", "FortranViewNode",
    "Coastlines", "Axis", "LegendVisitor", "TextVisitor", "ImportObjectHandler",
    "GribDecoder", "NetcdfDecoder", "InputData", "TableDecoder", "WrepJSon"
    ]

check = ["GraphPlotting"]
list = all
list = check
"""
minicolor = [
    ["jquery.miniColors.js",  "Reference guide", "MiniColor"],
    ["jquery.miniColors.css", "Reference guide", "MiniColor"]
]
"""
from tojson import  createAction
#put("magdoc.js",     "Reference guide", "Magics gallery generator")
for i in list:
    definition = action[i]
    createAction("2.31.0", definition["json"], i, definition["files"])
    put(definition["json"]+".json", definition["jira"],  definition["doc"])
