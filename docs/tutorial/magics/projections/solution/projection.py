# importing Magics module
from Magics.macro import *

#Setting of the output file name
output = output({'output_name':'projection'})

#Setting the coordinates of the geographical area
map=mmap({
	"subpage_upper_right_longitude": 180., 
	"subpage_upper_right_latitude": 90., 
	"subpage_lower_left_longitude": -180., 
	"subpage_map_projection": "cylindrical",
	"subpage_lower_left_latitude": -90.})

#Setting the coastlines attributes
coast = mcoast( {"map_coastline_sea_shade_colour": "white", 
		 "map_coastline_land_shade_colour": "cream", 
		 "map_grid": "on", 
		 "map_grid_colour": "grey", 
		 "map_coastline_land_shade": "on", 
		 "map_coastline_sea_shade": "on", 
		 "map_label": "off", 
		 "map_coastline_colour": "grey"})

#Setting of the title
text = mtext({'text_lines': ["Global Map!"]})

plot(output, map, coast, text)
