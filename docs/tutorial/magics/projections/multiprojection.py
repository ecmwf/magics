# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module
from Magics.macro import *

# Definitions of the projection using a dictionary to 
# organise the informations
cylindrical = { "project" : "global",
		        "text" : "Global Cylindrical",
		        "map" : mmap({
				"subpage_upper_right_longitude": 180., 
				"subpage_upper_right_latitude": 90., 
				"subpage_lower_left_longitude": -180., 
				"subpage_map_projection": "cylindrical", 
				"subpage_lower_left_latitude": -90.})
	      }

# Add the other areas here !

# Create the list of the areas 
projections=[cylindrical]

# Create a empty list to store the objects to plot!
toplot = []

# Setting of the output file name
output = output({'???':'multi_projection'})

# store the output in the list

toplot = toplot + [output]

#Loop on the projections
for projection in projections:

# Create a new page
	newpage=page()

# Setting the coordinates of the geographical area
	map = projection["???"]


# Setting the coastlines attributes
	coast = mcoast( {"map_coastline_sea_shade_colour": "white", 
			"map_coastline_land_shade_colour": "cream", 
			"map_grid": "on", 
			"map_grid_colour": "grey", 
			"map_coastline_land_shade": "on", 
			"map_coastline_sea_shade": "on", 
			"map_label": "off", 
			"map_coastline_colour": "grey"})

#Setting of the title
	text = mtext({'text_lines': [projection["???"]])

#Add to the list of thing to plot
	toplot = toplot +[newpage, ???, ???, ???]

#Plot the result
???(toplot)
