#importing Magics module
from Magics.macro import *


ref = 'polder'
#Setting of the output file name
output = output(output_formats = ['png'],output_name_first_page_number = "off",output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'cylindrical',
		  subpage_lower_left_latitude= 55.,
		  subpage_lower_left_longitude= -9.,
		  subpage_upper_right_latitude= 60.,
		  subpage_upper_right_longitude= -1.)

#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
		map_coastline_land_shade  =  'on',
		map_coastline_land_shade_colour  =  'cream',
		map_coastline_colour =  "tan",
                map_coastline_resolution = "high",
		map_coastline_style = "dash")

#Title settings
title = mtext(
	  text_lines = ["<font size='1'>High resolution dash coastline</font>"],
	  text_justification = "left",
	  text_font_size = 0.8,
	  text_colour = "charcoal")

#To the plot
plot(output, projection, coast,title)

#For documentation only
tofortran(ref, output, projection, coast, title)
tomv4(ref,coast)
tohtml(ref,coast)














