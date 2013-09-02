
#importing Magics module
from Magics.macro import *

#Setting of the output file name
output = output( output_formats = ['ps', 'png'], 
        output_name_first_page_number = "off",
		output_name= 'advanced'})

#Setting the coordinates of the geographical area
#Here we use australia

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour = "white",
                     map_coastline_land_shade_colour =  "grey",
                     map_grid =  "on",
                     map_coastline_land_shade = "on",
                     map_coastline_sea_shade = "on",
                     map_label = "off",
                     map_coastline_colour = "black")


legend = mlegend(legend = ": "on", 
			legend_text_colour = ":"navy",
			legend_display_type = ": "histogram")

#Import the airep data 
airep =  mgeo( geo_input_file_name = "airep.geo")

#Define the simple contouring for msl
airep_symbol = msymb( legend = "on",
		    symbol_type = "marker",
			symbol_table_mode": "advanced",
			symbol_advanced_table_selection_type = "interval",
			symbol_advanced_table_interval = 5.,
			symbol_advanced_table_min_level_colour = "blue",
			symbol_advanced_table_max_level_colour = "red",
			symbol_advanced_table_colour_direction = "clockwise",
			symbol_marker_index =  15 )

lines =["Monitoring of airep data"]

title = mtext(text_lines = lines,
           text_html = "true",
           text_colour = "navy",
           text_font_size = 0.6,
           text_mode = "positional",
           text_box_x_position = 1.5,
           text_box_y_position = 16.5,
           text_box_x_length = 20.,
           text_box_y_length = 2.5,
           text_border = "off",
           text_justification = "left")


#To the plot
plot(output, background, airep, airep_symbol, title, legend)

#For the documentation
tofortran("advanced", output, background, airep, airep_symbol, title, legend)
tomv4("advanced", airep_symbol)
tohtml("advanced", airep_symbol)














