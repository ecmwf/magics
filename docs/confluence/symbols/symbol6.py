# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *

import random

ref = 'symbol6'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'regular',
			subpage_y_axis_type = 'regular',
			subpage_x_min = -10.,
			subpage_x_max = 10.,
			subpage_y_min = -10.,
			subpage_y_max = 10.)

#Vertical axis
vertical = maxis(axis_orientation = "vertical",
				 axis_type = "regular",
			     axis_tick_label_height = 0.4,
				 axis_tick_label_colour = 'navy',
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")

#Horizontal axis
horizontal = maxis(axis_orientation = "horizontal",
				 axis_type = "regular",
			     axis_tick_label_height = 0.4,
				 axis_tick_label_colour = 'navy',
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
x = [random.randint(-1000,1000)/100. for r in xrange(1000)]
y = [random.randint(-1000,1000)/100. for r in xrange(1000)]
values = [random.randint(-1000,1000)/100. for r in xrange(1000)]



input = minput(input_x_values = x,
			input_y_values = y, input_values= values)

#Define the graph 
symbol = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_height =  0.5,
			legend =  "on",
			legend_display_type =  "continuous",
			symbol_table_mode=  "advanced",
			symbol_marker_index = 15,
			symbol_advanced_table_selection_type = "interval",
			symbol_advanced_table_interval = 2.00,
			symbol_advanced_table_max_level_colour = "red",
			symbol_advanced_table_min_level_colour = "blue",
			symbol_advanced_table_colour_direction = "clockwise",
			symbol_advanced_table_height_list = [0.4],
			)

title = mtext(
           text_lines = ["Advanced symbol table mode plotting ..."],
		   text_justification = "left",
		   text_font_size = 1.,
		   text_colour =  "charcoal")



#To the plot
plot(output, projection, vertical, horizontal, input, symbol,  title)
tofortran(ref, output, projection, vertical, horizontal, input, symbol,  title)
tohtml(ref, input, symbol)
tomv4(ref, input, symbol)















