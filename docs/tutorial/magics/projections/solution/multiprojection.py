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

# Add the other areas here 
europe = { "project" : "europe",
           "text" : "Europe",
           "map" : mmap({
                  "subpage_lower_left_latitude": 21.51,
                  "subpage_lower_left_longitude": -37.27,
                  "subpage_upper_right_latitude": 51.28, 
                  "subpage_upper_right_longitude": 65., 
                  "subpage_map_projection": "polar_stereographic" })
         }

north_america = { "project" : "North America",
                  "text" : "North America",
                  "map" : mmap({
                    "subpage_map_vertical_longitude": -100., 
                    "subpage_lower_left_latitude": -5.,
                    "subpage_lower_left_longitude": -140.,
                    "subpage_upper_right_latitude": 30.,
                    "subpage_upper_right_longitude": -15.,
                    "subpage_map_projection": "polar_stereographic" })
                }

south_america = { "project" : "South America",
                  "text" : "South America",
                  "map" : mmap({
                    "subpage_map_vertical_longitude": 0., 
                    "subpage_lower_left_latitude": -65.,
                    "subpage_lower_left_longitude": -125.,
                    "subpage_upper_right_latitude": 20., 
                    "subpage_upper_right_longitude": 5., 
                    "subpage_map_projection": "cylindrical" })
                }

asia = { "project" : "Asia",
         "text" : "Asia",
         "map" : mmap({
                    "subpage_map_vertical_longitude": 0., 
                    "subpage_lower_left_latitude": 0.,
                    "subpage_lower_left_longitude": 55.,
                    "subpage_upper_right_latitude": 80., 
                    "subpage_upper_right_longitude": 175., 
                    "subpage_map_projection": "cylindrical" })
       }

australia = { "project" : "Australia",
              "text" : "Australia",
              "map" : mmap({
                    "subpage_map_vertical_longitude": 0., 
                    "subpage_lower_left_latitude": -55.,
                    "subpage_lower_left_longitude": 80.,
                    "subpage_upper_right_latitude": -5., 
                    "subpage_upper_right_longitude": 190., 
                    "subpage_map_projection": "cylindrical" })
            }

africa = { "project" : "Africa",
           "text" : "Africa",
           "map" : mmap({
                    "subpage_map_vertical_longitude": 0., 
                    "subpage_lower_left_latitude": -40.,
                    "subpage_lower_left_longitude": -45.,
                    "subpage_upper_right_latitude": 40., 
                    "subpage_upper_right_longitude": 75., 
                    "subpage_map_projection": "cylindrical" })
         }

# Create the list of the areas 
projections=[cylindrical, europe, north_america, south_america, asia, australia, africa]

# Create a empty list to store the objects to plot!
toplot = []

# Setting of the output file name
output = output({'output_name':'multi_projection'})

# store the output in the list

toplot = toplot + [output]

# Loop on the projections
for  projection in projections:

	# create a new page
	newpage = page()

	print projection

	# Setting the coordinates of the geographical area
	map=projection["map"]


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
	text = mtext({'text_lines': [projection["text"]]})

	#Add to the list of things to plot
	toplot = toplot + [newpage, map, coast, text]

# end of the loop

# Plot the result
plot(toplot)
