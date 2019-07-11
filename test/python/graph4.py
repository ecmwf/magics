# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'graph4'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2., 
			subpage_map_projection = 'cartesian',
			subpage_x_axis_type = 'date',
			subpage_y_axis_type = 'regular',
			subpage_x_date_min = "2012-03-03 12:00:00",
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
x =  ["2012-03-04 00:00:00","2012-03-04 12:00:00","2012-03-05 00:00:00"]
y_min = numpy.array([30.,35.,50.])
y_max = numpy.array([60.,40.,70.])

input_line = minput(input_y_values=y_min,
				input_y2_values=y_max,
				input_date_x_values=x,
				input_date_x2_values=x,)

#Define the graph 
line = mgraph( 
            graph_type="bar",
			graph_bar_justification = 'right',
			graph_bar_colour='red',
			graph_bar_line_colour='red',
			graph_bar_line_thickness=4,
			graph_bar_width=6*3600.,
			graph_bar_style = "linebar",
			legend =  "on",
			legend_user_text =  "<font size='0.5' colour='red'> Using linebar style  </font>"
			)

#define  the  data 
x =  ["2012-03-06 00:00:00","2012-03-06 12:00:00","2012-03-07 00:00:00"]
y = numpy.array([60.,40.,30.])

input_annotation = minput(input_y_values=y,
				input_y2_values=[0.,0.,0.],
				input_date_x_values=x,
				input_date_x2_values=x,)
#Define the graph 
annotation = mgraph( 
            graph_type="bar",
            graph_bar_annotation= ["<font colour='red'>red result</font>", "result", "<font colour='evergreen'>green result</font>"] ,
			graph_bar_justification = 'centre',
			graph_bar_colour='green',
			graph_bar_annotation_font_colour= "charcoal",
			graph_bar_annotation_font_size= 0.5,
			graph_bar_width=3*3600. ,
			legend =  "on",
			legend_user_text =  "<font size='0.5' colour='evergreen'> Using annotations </font>"
			)

title = mtext(
           text_lines = ["More options on bar plotting ..."],
		   text_justification = "left",
		   text_font_size = 1.,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, vertical, horizontal, 
		input_line, line, input_annotation, annotation, title)

#For documentation only
tofortran(ref, output, projection, vertical, horizontal, input_line, line, input_annotation, annotation, title)
tomv4(ref, line, annotation)
tohtml(ref, input_line, line, input_annotation, annotation, )














