# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'graph3'
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
				 axis_grid =  "on",
				 axis_days_label_height = 0.40,
			 	 axis_months_label_height = 0.40,
				 axis_years_label_height = 0.50,
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
x =  ["2012-03-02 00:00:00","2012-03-02 12:00:00","2012-03-03 00:00:00"]
y = numpy.array([35.,45.,55.])

input_centre = minput(input_y_values=y,
			input_date_x_values= x,)
#Define the graph 
centre = mgraph( 
            graph_type="bar",
			graph_bar_justification = 'centre',
			graph_bar_colour='blue',
			graph_bar_width=3*3600.,
			legend =  "on",
			legend_user_text =  "<font colour='blue'> Centered </font>"
			)


			
#define  the  data 
x =  ["2012-03-04 00:00:00","2012-03-04 12:00:00","2012-03-05 00:00:00"]
y = numpy.array([60.,40.,30.])

input_right = minput(input_y_values=y,
			input_date_x_values= x,)
			
#Define the graph 
right = mgraph( 
            graph_type="bar",
			graph_bar_justification = 'right',
			graph_bar_colour='red',
			graph_bar_width=6*3600.,
			legend =  "on",
			legend_user_text =  "<font colour='red'> Right justified </font>"
			)

#define  the  data 
x =  ["2012-03-06 00:00:00","2012-03-06 12:00:00","2012-03-07 00:00:00"]
y = numpy.array([60.,40.,30.])

input_left = minput(input_y_values=y,
			input_date_x_values= x,)

#Define the graph 
left = mgraph( 
            graph_type="bar",
			graph_bar_justification = 'left',
			graph_bar_colour='green',
			graph_bar_width=3*3600. ,
			legend =  "on",
			legend_user_text =  "<font colour='evergreen'> Left justified </font>"
			)

title = mtext(
           text_lines = ["Bar Justification"],
		   text_justification = "left",
		   text_font_size = 1.,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, vertical, horizontal, input_left, left, 
			input_right, right, input_centre, centre, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input_left, left, 
			input_right, right, input_centre, centre, title)
tomv4(ref, left, right, centre)
tohtml(ref, input_left, left )














