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
ref = 'legend2'

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
		     contour_shade_max_level= 48.,
		     contour_shade_min_level= -48.,		     
		     contour_level_selection_type = "level_list",
		     contour_level_list = [-48.0,-10.0,0.0,10.0,20.0,48.0],
		     contour_shade_colour_method= "list",
		     contour_shade_colour_list= [ "blue_purple",
						  "greenish_blue",
		                                  "blue_green",
		                                  "yellow_green",
		                                  "yellow",		                                  
		                                  "orange" ])


title = mtext(text_lines = ["<font size='1'>Disjoint legend with user defined labels</font>",""],
	      text_justification = "left",
	      text_font_size = 0.5,
	      text_colour = "charcoal")

#add a legend
legend = mlegend(legend = "on",
		 legend_text_colour="black",
		 legend_display_type= "disjoint",
		 legend_title = "on",
		 legend_title_text= "Temperature at 850 hPa",
		 legend_text_font_size = "0.5",
		 legend_entry_text_width = 80.0,
		 legend_text_composition = "user_text_only",
		 legend_user_lines = ["extremely cold",
		                      "very cold",
		                      "cold",
		                      "temperate",
		                      "hot"])

#To the plot
plot(output, europe, t850, t850_contour, coast, title, legend)

tofortran(ref,output, europe, t850, t850_contour, coast, title, legend)
tomv4(ref,legend)
tohtml(ref,legend)