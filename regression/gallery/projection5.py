# importing Magics module
from Magics.macro import *

# Definitions of the projection using a dictionary to 
# organise the informations
projection = { "name": "projection5",
               "project" : "Lambert",
               "text" : "Lambert Conformal Conic Projection ( using proj4 package) ",
                "map" : mmap(
				    subpage_map_area_definition = "corners",
                    subpage_lower_left_latitude =  -0.,
                    subpage_lower_left_longitude =  -80.,
                    subpage_upper_right_latitude =  70., 
                    subpage_upper_right_longitude =  160., 
                    subpage_map_projection =  "lambert")
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
