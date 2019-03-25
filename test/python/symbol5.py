# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


ref = 'symbol5'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-03-01 12:00:00",
			subpage_x_date_max = "2012-03-07 12:00:00",
			subpage_y_min = 20.,
			subpage_y_max = 100.)

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
				 axis_type = "date",
				 axis_days_label_height = 0.40,
			 	 axis_months_label_height = 0.40,
				 axis_years_label_height = 0.50,
				 axis_minor_tick =  "on",
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
x =  ["2012-03-02 00:00:00","2012-03-03 12:00:00","2012-03-05 00:00:00"]
bottom = numpy.array([30.,30.,30.])
left = numpy.array([45.,45.,45.])
right = numpy.array([60.,60.,60.])
top = numpy.array([75.,75.,75.])
middle = numpy.array([90.,90.,90.])


topinput = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = top)

#Define the graph 
toptext = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_text_list = ["top", "top", "top"],
			symbol_height =  1.,
			symbol_text_font_size =  0.8,
			symbol_text_font_colour =  "black",
			symbol_text_position =  "top",
			symbol_marker_index = 15
			)


leftinput = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = left)

#Define the graph 
lefttext = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_text_list = ["left", "left", "left"],
			symbol_height =  1.,
			symbol_text_font_size =  0.8,
			symbol_text_font_colour =  "black",
			symbol_text_position =  "left",
			symbol_marker_index = 15
			)

rightinput = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = right)

#Define the graph 
righttext = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_text_list = ["right"],
			symbol_height =  1.,
			symbol_text_font_size =  0.8,
			symbol_text_font_colour =  "black",
			symbol_text_position =  "right",
			symbol_marker_index = 15
			)

bottominput = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = bottom)

#Define the graph 
bottomtext = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_text_list = ["bottom"],
			symbol_height =  1.,
			symbol_text_font_size =  0.8,
			symbol_text_font_colour =  "black",
			symbol_text_position =  "bottom",
			symbol_marker_index = 15
			)

leftinput = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = left)

#Define the graph 
lefttext = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_text_list = ["left", "left", "left"],
			symbol_height =  1.,
			symbol_text_font_size =  0.8,
			symbol_text_font_colour =  "black",
			symbol_text_position =  "left",
			symbol_marker_index = 15
			)

centreinput = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = middle)

#Define the graph 
centretext = msymb( 
			symbol_type =  "marker",
			symbol_colour =  "red",
			symbol_text_list = ["a", "b", "centre"],
			symbol_height =  1.2,
			symbol_text_font_size =  0.8,
			symbol_text_font_colour =  "black",
			symbol_text_position =  "centre",
			symbol_marker_index = 15
			)

title = mtext(
           text_lines = ["Using marker and text ..."],
		   text_justification = "left",
		   text_font_size = 1.,
		   text_colour =  "charcoal")



#To the plot
plot(output, projection, vertical, horizontal, topinput, toptext, 
		leftinput, lefttext, 
		bottominput, bottomtext,
		rightinput, righttext, 
		centreinput, centretext, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, 
        topinput, toptext, 
		leftinput, lefttext, 
		bottominput, bottomtext,
		rightinput, righttext, 
		centreinput, centretext, title)
        
tomv4(ref, topinput, toptext)
tohtml(ref, topinput, toptext)














