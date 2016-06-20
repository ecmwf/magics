# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module
from Magics.macro import *

ref = 'contour6'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)


#Setting the coordinates of the geographical area
projection = mmap(subpage_upper_right_longitude = 65.00,
        		subpage_upper_right_latitude = 51.28,
		        subpage_lower_left_latitude = 21.51,
			    subpage_lower_left_longitude = -37.27,
			    subpage_map_projection = 'polar_stereographic')

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='on',
               map_coastline_land_shade_colour='cream',
               map_coastline_colour='tan')

# Import the data
data = mgrib(grib_input_file_name='t850.grb')

# Define a contour 
list = mcont(contour_highlight='off',
			contour_level_selection_type='interval',
			contour_interval=1.,
			contour_line_colour_rainbow='on',
			contour_line_colour_rainbow_method='list',
			contour_line_colour_rainbow_colour_list=["orange", 'green', 'pink', 'navy'],
		    contour_line_colour_rainbow_colour_list_policy='cycle')

title = mtext(
           text_lines = ["<font size='1'>Rainbow technique to colour the isolines </font>",
					"using a user-defined list of colours",
		   			"<font colour='evergreen'>contour_line_colour_rainbow = on</font> ", 
					"<font colour='evergreen'>contour_line_colour_rainbow_method = list</font>"],
		   text_justification = "left",
		   text_font_size = 0.8,
           text_colour =  "charcoal")

# To the plot
plot(
    output, 
    projection,
    coast, data, list,
    title
    )


#For documentation only
tofortran(ref, output, projection,  coast, data, list, title)
tomv4(ref, list)
tohtml(ref, list)
