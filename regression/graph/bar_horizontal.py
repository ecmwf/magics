
#importing Magics module
from Magics.macro import *


ref = 'bar_horizontal'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'regular',
			subpage_y_axis_type = 'regular',
			subpage_x_min = 10.,
			subpage_x_max = 40.,
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
				 axis_grid =  "on",
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")



#define  the  data 
x =  numpy.array([20.,30.,15.])
y = numpy.array([50.,30.,40.])

input = minput(input_y_values=y,
	       input_x_values= x,)
#Define the graph 
graph = mgraph( 
            graph_type="bar",
			graph_bar_colour='evergreen',
			graph_bar_width=2.,
            graph_bar_orientation='horizontal')



title = mtext(
           text_lines = ["Simple Bar"],
		   text_justification = "left",
		   text_font_size = 1.,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, vertical, horizontal, input, graph, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input, graph, title)
tomv4(ref, graph)
tohtml(ref, input, graph)














