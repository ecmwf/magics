# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *

#Setting of the output file name
output = output({"output_formats":['ps', 'png'], 
		'output_name': 'simple_symbol'})

#Setting the coordinates of the geographical area
#Here we use australia

#Background Coastlines 
background = mcoast( {"map_coastline_sea_shade_colour": "white",
                     "map_coastline_land_shade_colour": "grey",
                     "map_grid": "off",
                     "map_coastline_land_shade": "on",
                     "map_coastline_sea_shade": "on",
                     "map_label": "off",
                     "map_coastline_colour": "black"})



#Import the airep data 
airep =  mgeo({ "geo_input_file_name" : "../airep.geo"})

#Define the simple contouring for msl
airep_symbol = msymb( {"symbol_type" : "marker",
		    "symbol_colour" : "evergreen", 
			"symbol_height" : 0.1,
			"symbol_marker": 15 })

lines =["My first symbol plotting"]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
           "text_mode": "positional",
           "text_box_x_position": 1.5,
           "text_box_y_position": 13.5,
           "text_box_x_length": 20.,
           "text_box_y_length": 2.5,
           "text_border": "on",
           "text_border_colour": "black",
           "text_box_blanking": "on",
           "text_justification" : "left"})


#To the plot
plot(output, background, airep, airep_symbol, title)














