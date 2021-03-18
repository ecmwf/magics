# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'contour4'

#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_upper_right_longitude = 65.00,
        		subpage_upper_right_latitude = 51.28,
		        subpage_lower_left_latitude = 21.51,
			    subpage_lower_left_longitude = -37.27,
			    subpage_map_projection = 'polar_stereographic')



#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
				map_coastline_land_shade  =  'on',
				map_coastline_land_shade_colour  =  'cream',
				map_coastline_colour =  "tan")


#Import the t850 data 
t850 =  mgrib(grib_input_file_name  = "t850.grb",
              grib_field_position =  1)


#Define a red contour for the positif values
positif = mcont( legend = "off",
				contour_level_selection_type = "interval",
				contour_interval=5.,
				contour_min_level=2.,
                contour_line_colour = "red",
                contour_line_thickness =  2,
                contour_label =  "on",
                contour_label_height =  0.3,
				contour_highlight="off")


#Define a blue contour for the negative values
negatif = mcont( legend = "off",
				contour_level_selection_type = "interval",
				contour_interval=5.,
				contour_max_level=-2.,
				contour_min_level=-1000.,
                contour_line_colour = "blue",
                contour_line_thickness =  2,
                contour_label =  "on",
                contour_label_height =  0.4,
				contour_highlight="off")



#Define a blue contour for the negative values
zero = mcont( legend = "off",
				contour_min_level=-1.,
				contour_max_level=1.,
				contour_level_selection_type = "level_list",
				contour_level_list=[0.],
                contour_line_colour = "black",
                contour_line_thickness =  4,
                contour_label =  "off",
				contour_highlight="off")


title = mtext(
           text_lines = ["Positive values are <font colour='red'>red </font>",
		   			"Negative values are <font colour='blue'>blue </font>",
		   			"0 is a black line "],
		   text_justification = "left",
		   text_font_size = 0.8,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, coast, t850, positif, t850, negatif, t850, zero, title)

#For documentation only
tofortran(ref, output, projection, coast, t850, positif, negatif, zero, title)
tomv4(ref, positif, negatif, zero)
tohtml(ref, positif, negatif, zero)














