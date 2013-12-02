
#importing Magics module
from Magics.macro import *


ref = 'graph8'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection1 = mmap(page_y_position = 0.,
			page_y_length = 7.,
		    subpage_y_position = 2., 
		    subpage_y_length = 5., 
		    subpage_frame = "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-05-25",
			subpage_x_date_max = "2012-06-07",
			subpage_y_min = 00.,
			subpage_y_max = 30.)

#Horizontal axis
horizontal_label = maxis(axis_orientation = "horizontal",
				 axis_type =  "date",
				 axis_grid =  "on",
				 axis_grid_colour =  "grey",
				 axis_minor_tick =  "on",
				 axis_date_type = "automatic",
				 axis_days_label_height =  0.4,
				 axis_hours_label_height =  0.4,
				 axis_months_label_height =  0.4,
				 axis_years_label_height =  0.4)



dates = ["2012-05-25 12:00:00", "2012-05-27 00:00:00", "2012-05-29 06:00:00", "2012-06-02 00:00:00", "2012-06-04 12:00:00"]
min = [ 3., 5., 7., 12., 5.]
max = [ 15., 18., 14., 25., 18.]


vertical =  maxis(axis_orientation = "vertical", 
				 axis_type =  "regular",
				 axis_tick_label =  "on",
				 axis_minor_tick =  "off",
				 axis_grid =  "on",
                 axis_grid_colour =  "grey")
horizontal = maxis(axis_orientation = "horizontal",
				 axis_type =  "date",
				 axis_date_type = "automatic",
				 axis_tick_label =  "off",
				 axis_grid =  "on",
				 axis_grid_colour =  "grey",
				 axis_tick = "off")

min_input =  minput( input_x_type = "date",
			input_date_x_values = dates,
			input_y_values = [ 3., 5., 7., 12., 5.])

max_input =  minput( input_x_type = "date",
			input_date_x_values = dates,
			input_y_values = [ 15., 18., 14., 25., 18.])


min_graph = mgraph(graph_line_colour  = "blue",
            graph_line_thickness = 8)
max_graph = mgraph(graph_line_colour  = "red",
            graph_line_thickness = 8)


page1 = page()
#Setting the cartesian view
projection2 = mmap( page_id_line = 'off',
			page_y_position = 7.,
			page_y_length = 5.,
		    subpage_y_position= 0., 
		    subpage_y_length= 5., 
		    subpage_frame= "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-05-25",
			subpage_x_date_max = "2012-06-07",
			subpage_y_min = -20.,
			subpage_y_max = 20.)


wind = minput(input_x_type = "date",
            input_date_x_values = dates,
			input_y_component_values = [20.00, -20.00, 20.00, 10., -10.],
			input_x_component_values = [20.00, 20.00, -20.00, 0., 0.],
			input_y_values = [0., 0., 0., 0., 0])


flags = mgraph(graph_type = "flag",
	graph_flag_colour = "black",
	graph_flag_length = 1.5
	)


speed = minput(input_x_type = "date",
            input_date_x_values = dates,
			input_y_values = [12., 3., 19., 7., 12.])


speed_graph = mgraph(graph_type = "curve",
	graph_line_colour = "evergreen",
	graph_line_thickness = 3
	)



page2 = page()


#Setting the cartesian view
projection3 = mmap( page_id_line = 'off',
			page_y_position = 12.,
			page_y_length = 5.,
		    subpage_y_position= 0., 
		    subpage_y_length= 5., 
		    subpage_frame= "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-05-25",
			subpage_x_date_max = "2012-06-07",
			subpage_y_min = 0.,
			subpage_y_max = 100.)

bar = mgraph(graph_type = "bar", 
			 graph_bar_justification = "centre",
			 graph_bar_y_upper_values = [29., 57., 35., 77., 12.],
			 graph_curve_date_x_values = dates,
			 graph_bar_colour = "navy",
			 graph_bar_width = 10800.00)

title = mtext(
           text_lines = ["Graph and box ..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")



#To the plot
plot(output, projection1, horizontal_label, vertical, min_input, min_graph, max_input, max_graph,
		page1,  projection2, horizontal, vertical, wind, flags, speed, speed_graph ,
		page2, projection3, horizontal, vertical, bar, title
		)

#For  the documentation only
tofortran(ref, output, projection1, horizontal_label, vertical, min_input, min_graph, max_input, max_graph,
		page1,  projection2, horizontal, vertical, wind, flags, speed, speed_graph ,
		page2, projection3, horizontal, vertical, bar, title
		)
tomv4(ref, vertical, min_input)
tohtml(ref, vertical, min_input)

