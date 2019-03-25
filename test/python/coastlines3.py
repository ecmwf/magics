# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


ref = 'coastlines3'
#Setting of the output file name
output = output(output_formats = ['png'],output_name_first_page_number = "off",output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'cylindrical',
		  subpage_lower_left_latitude= 34.,
		  subpage_lower_left_longitude= -10.,
		  subpage_upper_right_latitude= 60.,
		  subpage_upper_right_longitude= 34.)

#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
		map_coastline_land_shade  =  'on',
		map_coastline_land_shade_colour  =  'cream',
		map_coastline_colour =  "tan",
                map_coastline_resolution = "high",
		map_boundaries = "on",
		map_boundaries_colour = "red",
		map_rivers = "on",
		map_grid_latitude_reference = 0.0,
		map_grid_latitude_increment = 2.0,
		map_grid_longitude_increment = 2.0,
		map_grid_line_style = "dot")


title = mtext(
	  text_lines = ["<font size='1'>Grid lines, boundaries and rivers</font>"],
	  text_justification = "left",
	  text_font_size = 0.8,
	  text_colour = "charcoal")

#To the plot
plot(output, projection, coast,title)

#For documentation only
tofortran(ref, output, projection, coast, title)
tomv4(ref,coast)
tohtml(ref,coast)















