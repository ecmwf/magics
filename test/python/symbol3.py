# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'symbol3'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the coordinates of the geographical area
#Here we use Europe
area = mmap(subpage_lower_left_latitude= 40.,
            subpage_lower_left_longitude= -20.,
			subpage_upper_right_latitude= 65.,
			subpage_upper_right_longitude= 10.)

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour = "white",
                     map_coastline_land_shade_colour = "cream",
                     map_grid = "on",
                     map_coastline_land_shade = "on",
                     map_coastline_sea_shade = "on",
                     map_label = "on",
                     map_coastline_colour = "tan")



#Import the input data 
input =  mgeo(geo_input_file_name = "input.geo")

#Define the symbol plotting
symbol = msymb( legend = "off", 
			symbol_type =  "marker",
		    symbol_colour =  "orange", 
			symbol_height =  1.,
			symbol_marker_mode = "name", 
			symbol_marker_name = "ww_80")

lines =["Using Shower Symbol"]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
		   "text_justification": "left",
		   "text_font_size": 1.,
           "text_colour" : "charcoal"})


#To the plot
plot(output, area, background, input, symbol, title)

#For documentation only
tofortran(ref, output, area, background, input, symbol, title)
tomv4(ref, symbol)
tohtml(ref, symbol)














