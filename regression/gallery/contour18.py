from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='period_of_swell.grib',grib_field_position=1)

#Setting output
output = output(
	output_formats                = ['png'],
	output_name                   = 'contour18',
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude    = -11.441861,
	subpage_lower_left_longitude   = 94.509331,
	subpage_map_projection         = 'cylindrical',
	subpage_upper_right_latitude   = 6.404417,
	subpage_upper_right_longitude  = 141.521805,
)    

#Setting the coastlines
background = mcoast(
	map_coastline_land_shade        = 'on',
    map_coastline_land_shade_colour = 'cream')

foreground = mcoast(
	map_coastline_land_shade        = 'off',
	map_grid_line_style             = 'dash',
	map_grid_colour                 = 'grey',
	map_label                       = 'on',
	map_coastline_colour            = 'black')

#Defining the contour
contour = mcont(
	contour_highlight              = 'off',
	contour_hilo                   = 'off',
	contour_label                  = 'off',
	contour_label_colour           = 'black',
	contour_label_format           = '(F4.1)',
	contour_level_list             = [0.0, 3.5, 5.0, 6.5, 8.0, 9.5, 11.0, 12.5, 14.0, 15.5, 17.0, 18.5],
	contour_level_selection_type   = 'level_list',
	contour_line_colour            = 'white',
	contour_min_level              = 0.0,
	contour_reference_level        = 0.0,
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['blue', 'greenish_blue', 'sky', 'yellowish_green', 'yellow', 'orange_yellow', 'yellowish_orange', 'orange', 'red', 'brown', 'charcoal'],
	contour_shade_colour_method    = 'list',
	contour_shade_method           = 'area_fill',
	contour_shade_min_level        = 0.0,
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">period of swell (Range: 0 .. 185)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour18',output,area,data,background,contour,foreground,title)
tomv4('contour18',contour)
tohtml('contour18',data,contour)