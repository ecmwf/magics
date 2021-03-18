# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *

#Example reference
ref = 'legend3'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= "off",
                output_name= ref)

#Setting the coordinates of the geographical area
europe = mmap(subpage_upper_right_longitude= 65.,
              subpage_map_projection= "polar_stereographic",
              subpage_map_vertical_longitude= 0.0,
              subpage_lower_left_longitude= -37.27,
              subpage_lower_left_latitude= 21.51,
              subpage_upper_right_latitude= 51.28)

#Coastlines setting
coast = mcoast( map_grid= "on",
                map_grid_colour= "tan",
                map_coastline_colour= "tan",
                map_coastline_resolution= "high")

#Import the z500 data 
z500 =  mgrib(grib_input_file_name = "./z500.grb",
              grib_id= "z500")

#Define the simple contouring for z500
z500_contour = mcont(legend= "off",
                     contour_level_selection_type= "interval",
                     contour_line_colour= "black",
                     contour_hilo_height= 0.25,
                     contour_line_thickness= 1,
                     contour_hilo= "on",
                     contour_hilo_quality= "high",
                     contour_label= "off",
                     contour_highlight_colour= "black",
                     contour_highlight_thickness= 2,
                     contour_interval= 5.)

#Import the t850 data 
t850 =  mgrib( grib_input_file_name = "./t850.grb",
               grib_id= "t850")


#Define the shading for t850
t850_contour = mcont(legend="on",
		     contour_shade= "on",
		     contour_hilo= "off",
		     contour= "off",
		     contour_label= "off",
		     contour_shade_method= "area_fill",
		     contour_level_selection_type = "level_list",
		     contour_level_list = [-10.0,-8.0,-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0,8.0,10.0],
		     contour_shade_max_level_colour= "red",
		     contour_shade_min_level_colour= "blue",
		     contour_shade_colour_direction= "clockwise")

    


title = mtext(text_lines = ["<font size='1'>Positional and histogram legend</font>","","","","",""],
	      text_justification = "left",
	      text_font_size = 0.5,
	      text_colour = "charcoal")

#add a legend
legend = mlegend(legend = "on",
 		 legend_text_colour="black",
		 legend_box_mode= "positional",
		 legend_box_x_position= 1.0,
		 legend_box_y_position= 16.0,
		 legend_box_x_length= 27.,
		 legend_box_y_length= 3., 		 
		 legend_display_type = "histogram",
		 legend_label_frequency = 1,
		 legend_histogram_mean_value = "on",
		 legend_title = "on",
		 legend_title_text= "Temperature at 850 hPa",
		 legend_text_font_size = 0.5)

#To the plot
plot(output, europe, t850, t850_contour, z500, z500_contour, coast, title, legend)

#For documentation only
tofortran(ref, output, europe, t850, t850_contour, z500, z500_contour, coast, title, legend)
tomv4(ref,legend)
tohtml(ref,legend)

