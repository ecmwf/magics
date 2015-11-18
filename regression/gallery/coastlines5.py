#importing Magics module
from Magics.macro import *


ref = 'coastlines5'
#Setting of the output file name
output = output(output_formats = ['png'],output_name_first_page_number = "off",output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'polar_stereographic')

#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
		map_coastline_land_shade  =  'on',
		map_coastline_land_shade_colour  =  'cream',
		map_coastline_colour =  "tan",
		map_coastline_style = "dash",
        map_label_font= "times",
        map_label_font_style= "italic",
        map_label_colour= "evergreen",
        map_label_blanking= "off",
        map_label_height= 0.6,
        )


#Title settings
title = mtext(
	  text_lines = ["<font size='1'>Test map_label attributes</font>"],
	  text_justification = "left",
	  text_font_size = 0.8,
	  text_colour = "charcoal")

#To the plot
plot(output, projection, coast,title)















