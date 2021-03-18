# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'contour3'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_upper_right_longitude = 50.00,
        		subpage_upper_right_latitude = 65.00,
		        subpage_lower_left_latitude = 25.00,
			    subpage_lower_left_longitude = -20.00,
			    subpage_map_projection = 'cylindrical')



#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
				map_coastline_land_shade  =  'on',
				map_coastline_land_shade_colour  =  'cream',
				map_coastline_colour =  "tan")


#Import the t850 data 
t850 =  mgrib(grib_input_file_name  = "t850.grb",
              grib_field_position =  1)



#Define a contour using a predefined contour level_list
contour = mcont( legend = "off",
				contour_level_selection_type = "level_list",
				contour_level_list=[-20., -10., -5., -2.5, -1, -0.5, 0., 0.5, 1, 2.5, 5, 10, 20],
                contour_line_colour = "grey",
                contour_line_thickness =  2,
                contour_label =  "off",
				contour_highlight="off",
                contour_grid_value_plot = "on",
                contour_grid_value_plot_type = "both",
                contour_grid_value_lat_frequency = 10, 
                contour_grid_value_lon_frequency = 10,
                contour_grid_value_height = 0.3,
                contour_grid_value_marker_height = 0.2)
                


title = mtext(
           text_lines = ["<font size='1'>Display the grid values ...</font>",
		   			"<font colour='evergreen'>contour_level_selection_type = level_list</font> ", 
					"    uses a user-defined list of contour levels.",
					"[-20., -10., -5., -2.5, -1, -0.5, 0., 0.5, 1, 2.5, 5, 10, 20]"],
		   text_justification = "left",
		   text_font_size = 0.8,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, coast, t850, contour, title)

#For documentation only
tofortran(ref, output, projection, coast, t850, contour, title)
tomv4(ref, contour)
tohtml(ref, t850, contour)














