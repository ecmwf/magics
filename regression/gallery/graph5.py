
#importing Magics module
from Magics.macro import *


ref = 'graph5'
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
x =  ["2012-03-02 00:00:00","2012-03-04 12:00:00","2012-03-05 00:00:00"]
y = numpy.array([35.,45.,55.])

input1 = minput(input_y_values= y,
			input_date_x_values= x)
			
#Define the graph 
curve1 = mgraph( 
			graph_line_colour='blue',
			graph_line_thickness=4,
			legend =  "on",
			legend_user_text =  "<font colour='blue' size='0.5'> Curve 1 </font>"
			)


#define  the  data 
x =  ["2012-03-04 00:00:00","2012-03-04 12:00:00","2012-03-05 12:00:00"]
y = numpy.array([60.,70.,30.])

input2 = minput(input_y_values= y,
			input_date_x_values= x)
#Define the graph 
curve2 = mgraph( 
			graph_line_colour='red',
			graph_line_thickness=4,
			legend =  "on",
			legend_user_text =  "<font colour='red' size='0.5'> Curve 2 </font>"
			)


title = mtext(
           text_lines = [" 2 curves on the same Graph"],
		   text_justification = "left",
		   text_font_size = 1.,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, vertical, horizontal, input1, curve1, input2, curve2, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input1, curve1, input2, curve2, title)
tomv4(ref, curve1, curve2)
tohtml(ref, input1, curve1 )














