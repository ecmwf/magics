# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


ref = 'graph7'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'regular',
			subpage_y_axis_type = 'regular',
			subpage_x_min = -50.,
			subpage_x_max = +50.,
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
				 axis_type = "regular",
			     axis_tick_label_height = 0.4,
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
x = numpy.array([-25.,0.,25.])
y = numpy.array([50.,35.,60.])
u = numpy.array([20.,20.,-20.])
v = numpy.array([20.,-20.,20.])


input = minput( input_x_values = x,
			input_y_values = y,
			input_x_component_values = u,
			input_y_component_values = v)

#Define the graph 
arrows = mgraph( 
			legend='on',
			graph_type = 'arrow',
			graph_arrow_colour = 'evergreen',
			graph_arrow_unit_velocity = 10.,
			legend_user_text =  "<font colour='evergreen' size='0.5'> Arrows </font>"
			)


title = mtext(
           text_lines = ["Plotting Arrows ..."],
		   text_justification = "left",
		   text_font_size = 1.,
		   text_colour =  "charcoal")



#T the plot
plot(output, projection, vertical, horizontal, input, arrows, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input, arrows, title)
tomv4(ref, input, arrows)
tohtml(ref, input, arrows )














