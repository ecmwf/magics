# importing Magics module
from Magics.macro import *

# Definitions of the projection using a dictionary to 
# organise the informations
projection = { "name": "projection2",
               "project" : "Europe",
                "text" : "Europe",
                "map" : mmap(
                  subpage_lower_left_latitude = 21.51,
                  subpage_lower_left_longitude =  -37.27,
                  subpage_upper_right_latitude =  51.28, 
                  subpage_upper_right_longitude =  65., 
                  subpage_map_projection =  "polar_stereographic" )
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
