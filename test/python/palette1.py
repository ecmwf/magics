#importing Magics module
from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='t850.grb',grib_field_position=1)

ref = 'palette1'

#Setting the output
output = output(
	output_formats = ['png'],
	output_name = ref,
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude = 35.,
	subpage_lower_left_longitude = -20.,
	subpage_map_projection = 'cylindrical',
	subpage_upper_right_latitude = 70.,
	subpage_upper_right_longitude = 35.,
)    

#Setting the coastlines
foreground = mcoast(
	map_coastline_land_shade = 'off',
	map_grid_line_style = 'dash',
	map_grid_colour = 'grey',
	map_label = 'on',
	map_label_height = 0.35,
	map_label_colour = 'charcoal',
	map_coastline_colour = 'charcoal')

#Defining the contour
contour = mcont(
	contour = 'off',
	contour_hilo = 'off',
	contour_interval = 2.0,
	contour_label = 'off',
	contour_level_selection_type = 'interval',
	contour_shade = 'on',
	contour_shade_palette_name = 'eccharts_rainbow_purple_red_25', 
	contour_shade_colour_method = 'palette',
	contour_shade_max_level = 22.0,
	contour_shade_method = 'area_fill',
	contour_shade_min_level = -28.0,
	legend = 'on'
)

#Picking the grib metadata
title = mtext(
    text_lines = ['<font size="1">Using predefined palette</font>', 
                  "contour_shade_colour_method = 'palette'", 
                  "contour_shade_palette_name = 'eccharts_rainbow_purple_red_25'",
                  '850 hPa temperature  (Range: -28 .. 22) '],
    text_justification = 'left',
    text_font_size = 0.6,
    text_colour = 'charcoal')     

#Adding the legend
legend = mlegend(
    	legend_display_type = 'continuous',
    	legend_box_mode = 'positional',
    	legend_box_x_position = 27.,
    	legend_box_y_position = 0.2,
    	legend_box_x_length = 3.,
    	legend_box_y_length = 17.5,
    	legend_text_font_size = 0.4,
    	legend_text_colour = 'charcoal'
    )

#Plotting
plot(output, area, data, contour, foreground, legend, title)

#For documentation only
tofortran(ref, output, area, data, contour, foreground, title)
tomv4(ref, contour)
tohtml(ref, data, contour)
