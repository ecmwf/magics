# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module
from Magics.macro import *

ref = 'contour7'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)


coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='on',
               map_coastline_land_shade_colour='cream',
               map_coastline_colour='tan')

# Import the data
data = mgrib(grib_input_file_name='t850.grb')

# Define a contour 
calculate = mcont(contour_highlight='off',
            contour_level_selection_type='interval',
            contour_interval=1.,
            contour_line_colour_rainbow='on',
            contour_line_colour_rainbow_method='calculate',
            contour_line_colour_rainbow_min_level_colour='blue',
            contour_line_colour_rainbow_max_level_colour='red',
            contour_line_colour_rainbow_direction='clockwise',
            )

title = mtext(
           text_lines = ["<font size='1'>Rainbow technique to colour the isolines </font>",
					"Computing a range of colours",
		   			"<font colour='evergreen'>contour_line_colour_rainbow = on</font> ", 
					"<font colour='evergreen'>contour_line_colour_rainbow_min_level_colour/contour_line_colour_rainbow_max_level_colour </font>"],
		   text_justification = "left",
		   text_font_size = 0.7,
           text_colour =  "charcoal")

# To the plot
plot(
    output, 
    coast, data, calculate,
    title
    )


#For documentation only
tofortran(ref, output,  coast, data, calculate, title)
tomv4(ref, calculate)
tohtml(ref, calculate)
