# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


ref = 'graph6'
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
			subpage_y_min = 25.,
			subpage_y_max = 75.)

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
y = numpy.array([35.,45.,55.])
u = numpy.array([0.,20.,-20.])
v = numpy.array([20.,0.,-20.])


input = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = y,
			input_x_component_values = u,
			input_y_component_values = v)

#Define the graph 
flags = mgraph( 
			legend = 'on',
			graph_type = 'flag',
			graph_flag_colour = 'pink',
			graph_flag_length = 2.,
			legend_user_text =  "<font colour='pink' size='0.5'> Flags </font>"
			)


title = mtext(
           text_lines = ["Plotting flags ..."],
		   text_justification = "left",
		   text_font_size = 1.,
		   text_colour =  "charcoal")



#T the plot
plot(output, projection, vertical, horizontal, input, flags, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input, flags, title)
tomv4(ref, input, flags)
tohtml(ref, input, flags )














