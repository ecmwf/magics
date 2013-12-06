
#importing Magics module
from Magics.macro import *


ref = 'axis4'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection1 = mmap(page_y_position = 0.,
			page_y_length = 4.,
		    subpage_y_position = 2., 
		    subpage_y_length = 1., 
		    subpage_frame = "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "1912-01-01",
			subpage_x_date_max = "2012-01-01",
			subpage_y_min = -20.,
			subpage_y_max = 20.)

#Horizontal axis
horizontal = maxis(axis_orientation = "horizontal",
				 axis_type =  "date",
				 axis_minor_tick =  "on",
				 axis_date_type = "automatic",
				 axis_days_label_height =  0.4,
				 axis_hours_label_height =  0.4,
				 axis_months_label_height =  0.4,
				 axis_years_label_height =  0.4)



title1 = mtext(
           text_lines = ["Using automatic labelling for long time serie..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")

page1 = page()
#Setting the cartesian view
projection2 = mmap( page_id_line = 'off',
			page_y_position = 13.,
			page_y_length = 4.,
		    subpage_y_position= 2., 
		    subpage_y_length= 1., 
		    subpage_frame= "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-05-01 06:00",
			subpage_x_date_max = "2022-05-02 12:00",
			subpage_y_min = -20.,
			subpage_y_max = 20.)

title2 = mtext(
           text_lines = [ "Using automatic labelling for 10 years time serie..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")


page2 = page()

#Setting the cartesian view
projection3 = mmap( page_id_line = 'off',
			page_y_position = 5.,
			page_y_length = 4.,
		    subpage_y_position= 2., 
		    subpage_y_length= 1., 
		    subpage_frame= "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2010-06-01",
			subpage_x_date_max = "2012-06-01",
			subpage_y_min = -20.,
			subpage_y_max = 20.)


title3 = mtext(
           text_lines = ["Using automatic labelling for One year time serie ..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")

page3 = page()

#Setting the cartesian view
projection4 = mmap( page_id_line = 'off',
			page_y_position = 9.,
			page_y_length = 4.,
		    subpage_y_position= 2., 
		    subpage_y_length= 1., 
		    subpage_frame= "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-05-01",
			subpage_x_date_max = "2012-05-10",
			subpage_y_min = -20.,
			subpage_y_max = 20.)

title4 = mtext(
           text_lines = ["Using automatic labelling for short time serie..."],
		   text_justification =  "left",
		   text_font_size =  0.5,
           text_colour =  "charcoal")

page4 = page()
#Setting the cartesian view
projection5 = mmap( page_id_line = 'off',
			page_y_position = 13.,
			page_y_length = 4.,
		    subpage_y_position= 2., 
		    subpage_y_length= 1., 
		    subpage_frame= "off", 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-05-01 06:00",
			subpage_x_date_max = "2012-05-02 12:00",
			subpage_y_min = -20.,
			subpage_y_max = 20.)

title5 = mtext(
           text_lines = ["<font size='0.8'> Automatic Method to setup labelling of date axis: [axis_date_type = </font><font size='0.8' colour='navy'>automatic</font><font size='0.8'>]</font>",
		   				"Using automatic labelling for very short time serie..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")


#To the plot
plot(output, projection1, horizontal, title1,
		page1,  projection2, horizontal, title2,
		page2, projection3, horizontal, title3,
		page3, projection4, horizontal, title4,
		page4, projection5, horizontal, title5,
		)

#For documentation only
tofortran(ref, output, projection1, horizontal, title1,
		page1, projection2, horizontal, title2,
		page2, projection3, horizontal, title3,
		page3, projection4, horizontal, title4,
		page4, projection5, horizontal, title5)
tomv4(ref, projection1, horizontal)
tohtml(ref, projection1, horizontal)



