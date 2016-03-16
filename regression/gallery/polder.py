#importing Magics module
from Magics.macro import *


ref = 'polder'
#Setting of the output file name
output = output(output_formats = ['png'],output_name_first_page_number = "off",output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'cylindrical',
		  subpage_lower_left_latitude= 51.,
		  subpage_lower_left_longitude= 4.,
		  subpage_upper_right_latitude= 53.,
		  subpage_upper_right_longitude= 8.)

#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
		map_coastline_land_shade  =  'on',
		map_coastline_land_shade_colour  =  'cream',
		map_coastline_sea_shade  =  'on',
		map_coastline_sea_shade_colour  =  'ecmwf_blue',
        map_coastline_resolution = "high",
		map_coastline_colour =  "tan",
		map_coastline_style = "solid")

#Title settings
title = mtext(
	  text_lines = ["<font size='1'>Polders should not be flooded</font>"],
	  text_justification = "left",
	  text_font_size = 0.8,
	  text_colour = "charcoal")

#To the plot
plot(output, projection, coast,title)














