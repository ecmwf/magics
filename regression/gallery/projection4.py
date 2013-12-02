# importing Magics module
from Magics.macro import *

# Definitions of the projection using a dictionary to 
# organise the informations
projection = { "name": "projection4",
               "project" : "Australia",
               "text" : "Australia",
                "map" : mmap(
                    subpage_map_vertical_longitude = 0., 
                    subpage_lower_left_latitude =  -55.,
                    subpage_lower_left_longitude =  80.,
                    subpage_upper_right_latitude =  -5., 
                    subpage_upper_right_longitude =  190., 
                    subpage_map_projection =  "cylindrical")
             }

# Setting of the output file name
output = output(output_name=projection["name"],
				output_formats = ['png'],
				output_name_first_page_number = "off"
			    )

# Setting the coordinates of the geographical area
map=projection["map"]


# Setting the coastlines attributes
coast = mcoast( 
			map_coastline_colour = "grey",
			map_coastline_land_shade =  "on", 
			map_coastline_land_shade_colour =  "cream", 
			map_grid =  "on", 
			map_grid_colour = "grey", 
			map_coastline_sea_shade  ="on", 
			map_coastline_sea_shade_colour=  "white", 
			map_label  ="off")

#Setting of the title
text = mtext( text_font_size = '0.7', 
		text_justification = "left",
		text_lines =  [projection["text"]])

# Plot the result
plot(output, map, coast, text)

# For documentation purposes
tofortran(projection["name"], output, map, coast, text)
tomv4(projection["name"], map)
tohtml(projection["name"],  map)
