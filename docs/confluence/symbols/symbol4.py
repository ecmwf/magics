#importing Magics module
from Magics.macro import *


ref = 'symbol4'
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


input = minput(input_x_type = 'date',
			input_date_x_values = x,
			input_y_values = y)

#Define the graph 
symbols = msymb( 
			legend = 'on',
			symbol_type =  "marker",
			symbol_marker_mode = "image",
			symbol_image_path='D96.png',
			symbol_image_format='png',
			symbol_colour =  "red",
			symbol_height =  1.,
			symbol_marker_index = 15
			)


title = mtext(
           text_lines = ["Using Symbol ..."],
		   text_justification = "left",
		   text_font_size = 1.,
		   text_colour =  "charcoal")

legend = mlegend(
			legend = "on",
			legend_user_text =  "<font colour='navy' size='0.7'> Pictograms </font>",
			legend_box_mode = "positional",
			legend_box_y_position = 16.5,
			legend_box_x_position = 20.00,
			legend_box_x_length = 5.00,
			legend_box_y_length = 2.00)



#To the plot
plot(output, projection, vertical, horizontal, input, symbols, title, legend)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input, symbols, title, legend)
tomv4(ref, input, symbols)
tohtml(ref, input, symbols )














