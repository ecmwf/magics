
import os

def put(file, where, comment):
	cmd = "java -jar /home/graphics/cgs/atlassian-cli-2.5.0/lib/confluence-cli-2.5.0.jar --server http://ussoftware.ecmwf.int:8081/wiki --user cgs --password loulou --action addAttachment --space MAGP --file '%s' --comment '%s' --title '%s' " % (file, comment, where)
	print cmd
	os.system(cmd)

"""
list = [
	["gallery.js",         "Reference guide", "Magics gallery generator"],
	["gallery/gallery.js", "Gallery", "Magics gallery generator"],
    ["magics.css", "Reference guide", "Magics table stylesheet"],
	["coast.json", "Coastlines", "Coastlines documentation"],
	["postscript.json", "Postscript output", "Postscript Driver documentation"],
	["png.json", "PNG output", "CAIRO Driver documentation"],
	["svg.json", "SVG output", "SVG Driver documentation"],
	["kml.json", "KML output", "KML Driver documentation"],
	["legend.json",    "Legend", "Legend documentation"],
	["input.json",    "Input Data", "Input Data  documentation"],
	["wrepjson.json", "WrepJSon", "WrepJSon documentation"],
	["coast.json",    "Coastlines", "Coastlines documentation"],
	["graph.json",    "Graph Plotting", "Graph Plotting documentation"],
	["grib.json",     "Grib Input", "Grib Input documentation"],
	["axis.json",     "Axis", "Axis documentation"],
	["cont.json",     "Contouring", "Contouring documentation"],
	["input.json",    "Input Data", "Input Data  documentation"],
	["import.json",   "Import Object", "Import facility documentation"],
	["magdoc.js",     "Reference guide", "Magics gallery generator"],
	["text.json",     "Text Plotting", " Text documentation"],
	["table.json",    "CSV Input", "CSV Input documentation"],
	["subpage.json", "Subpage",         "Subpage documentation"],
	["cont.json",     "Contouring", "Contouring documentation"],
	["symbol.json",     "Symbol", " Symbol Plotting documentation"],
	["graph.json",     "Graph Plotting", " Graph Plotting documentation"],
    ["netcdf.json", "Netcdf Input", "Netcdf Input documentation"]
	["wind.json", "Wind Plotting", "Wind Plotting documentation"],
    ["page.json", "Page", "Page documentation"],
    ["superpage.json", "Superpage", "Super Page documentation"]
]
"""

list = [
	["kml.json", "KML output", "KML Driver documentation"],
]

"""
minicolor = [ 
    ["jquery.miniColors.js",  "Reference guide", "MiniColor"],
    ["jquery.miniColors.css", "Reference guide", "MiniColor"]
]
"""

for i in list:	put(i[0], i[1], i[2])
