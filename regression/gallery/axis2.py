
#importing Magics module
from Magics.macro import *


ref = 'axis2'
#Setting of the output file name
output = output(output_formats = ['ps'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap(subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-04-10",
			subpage_x_date_max = "2012-05-10",
			subpage_y_min = -20.,
			subpage_y_max = 20.)

#Vertical axis
vertical = maxis(axis_orientation = "vertical",
				 axis_grid =  "on",
				 axis_type =  "regular",
				 axis_tick_label_font_style = "bold",
				 axis_tick_label_height = 0.6,
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_reference_level = 0.,
				 axis_grid_reference_line_style='solid',
				 axis_grid_reference_thickness=2,
				 axis_grid_line_style = "dot")

#Horizontal axis
horizontal = maxis(axis_orientation = "horizontal",
				 axis_type =  "date",
				 axis_grid =  "on",
				 axis_months_label_font_style = "italic",
				 axis_months_label_font = "times",
				 axis_days_label_font_style = "italic",
				 axis_days_label_font_name = "times",
				 #axis_tick_label_font_style = "bold",
                 axis_years_label_font_style = "italic",
				 axis_tick_label_height = 0.6,
				 axis_days_label_height =  0.6,
				 axis_months_label_height =  0.6,
				 axis_years_label_height =  0.6,
				 axis_grid_colour = "grey",
				 axis_grid_thickness = 1,
				 axis_grid_line_style = "dot")




lines =["Using time series..."]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
		   "text_justification": "left",
		   "text_font_size": 1.,
           "text_colour" : "charcoal"})


#To the plot
plot(output, projection, vertical, horizontal, title)

