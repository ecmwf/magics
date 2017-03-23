
#importing Magics module
from Magics.macro import *


ref = 'bar_horizontal2'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_y_axis_type = 'regular',
			subpage_x_axis_type = 'regular',
			subpage_y_min = 10.,
			subpage_y_max = 70.,
			subpage_x_min = 25.,
			subpage_x_max = 75.)

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
				 axis_grid =  "on",
				 axis_days_label_height = 0.40,
			 	 axis_months_label_height = 0.40,
				 axis_years_label_height = 0.50,
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
y =  numpy.array([15.,20.,25.])
x = numpy.array([35.,45.,55.])

input_centre = minput(input_y_values=y,
			input_x_values= x,)
#Define the graph 
centre = mgraph( 
            graph_type="bar",
            graph_bar_orientation='horizontal',
			graph_bar_justification = 'centre',
			graph_bar_colour='blue',
			graph_bar_width=2.,
			legend =  "on",
			legend_user_text =  "<font colour='blue'> Centered </font>"
			)


			
#define  the  data 
y =  numpy.array([35.,40.,45.])
x = numpy.array([60.,40.,30.])

input_right = minput(input_y_values=y,
			input_x_values= x,)
			
#Define the graph 
right = mgraph( 
            graph_type="bar",
			graph_bar_justification = 'right',
			graph_bar_orientation='horizontal',
			graph_bar_colour='red',
			graph_bar_width=2. ,
			legend =  "on",
			legend_user_text =  "<font colour='red'> Right justified </font>"
			)

#define  the  data 
y = numpy.array([55.,60.,65.])
x = numpy.array([60.,40.,30.])

input_left = minput(input_y_values=y,
			input_x_values= x,)

#Define the graph 
left = mgraph( 
            graph_type="bar",
			graph_bar_justification = 'left',
			graph_bar_colour='green',
			graph_bar_orientation='horizontal',
			graph_bar_width=2. ,
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














