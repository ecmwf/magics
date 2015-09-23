
#importing Magics module
from Magics.macro import *

#Example reference
ref = 'geojson_multilines'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
area = mmap(subpage_upper_right_longitude= 180.,
                 subpage_upper_right_latitude= 90.,
                 subpage_lower_left_longitude= -180.,
                 subpage_lower_left_latitude= -90.)

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'off',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'off',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast( map_grid= 'on',
                 map_grid_colour = 'tan',
                 map_label= 'off',
                 map_coastline_colour= 'tan',
                 map_coastline_land_shade= 'off',
                 map_coastline_sea_shade= 'off')

#Import the z500 data 
cold =  mgeojson( geojson_input_filename = 'cold_fronts.geojson',)
blue = mline(polyline_line_colour = "blue", polyline_line_thickness = 2)

warm =  mgeojson( geojson_input_filename = 'warm_fronts.geojson',)
red = mline(polyline_line_colour = "red", polyline_line_thickness = 2)


title = mtext(text_lines = ["<font size='1'>GeoJSon First example</font>",
	                    "",
	                    "Warm and cold fronts"],
	      text_justification = 'left',
	      text_font_size = 0.8,
	      text_colour = 'charcoal')

#add a legend
legend = mlegend(legend= 'on',
           legend_text_colour= 'black',
           legend_box_mode= 'positional',
           legend_box_x_position= 15.,
           legend_box_y_position= 0.5,
           legend_box_x_length= 4.,
           legend_box_y_length= 18.,
           legend_border= 'off',
           legend_border_colour= 'black',
           legend_box_blanking= 'on',
           legend_display_type= 'continuous',
           legend_title = "on",
	   legend_title_text= "Wind",
	   legend_text_font_size = "0.5")

#To the plot
plot(output, background, warm, red, cold, blue, foreground, title, legend)

