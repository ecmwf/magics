
#importing Magics module
from Magics.macro import *


ref = 'graph11'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap(subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'regular',
			subpage_y_axis_type = 'regular',
			subpage_x_min = -20.,
			subpage_x_max = 20.,
			subpage_y_min = 25.,
			subpage_y_max = 75.)

#Vertical axis
vertical = maxis(axis_orientation = "vertical",
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")

#Horizontal axis
horizontal = maxis(axis_orientation = "horizontal",
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
x = numpy.array([-15.,-10, -5, 0., 5, 15., 20, 30.])
y = numpy.array([50.,40,30.,50., 55, 60., 40, 35.])

#define the input data
input = minput(input_x_values =  x,
			input_y_values =  y)

#Define the graph 
graph = mgraph( graph_line_colour  = "red",
            graph_line_thickness = 8,
            graph_curve_method = "stepped",
            legend =  "on",
            legend_user_text =  "<font colour='red'> My red Histo </font>",
            )




title = mtext(
           text_lines = ["Simple Histo Style with legend"],
		   text_justification = "left",
		   text_font_size = 1.,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, vertical, horizontal, input, graph, title)















