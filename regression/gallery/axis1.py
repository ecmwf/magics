
#importing Magics module
from Magics.macro import *


ref = 'axis1'
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




lines =["Regular Cartesian View..."]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
		   "text_justification": "left",
		   "text_font_size": 1.,
           "text_colour" : "charcoal"})


#To the plot
plot(output, projection, vertical, horizontal, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, title)
tomv4(ref, vertical)
tohtml(ref, projection, vertical, horizontal)














