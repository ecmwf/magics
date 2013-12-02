from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='Total_precipitation.grib',grib_field_position=1)

#Setting output
output = output(
	output_formats                = ['png'],
	output_name                   = 'contour11',
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude    = -34.250706,
	subpage_lower_left_longitude   = -74.485535,
	subpage_map_projection         = 'cylindrical',
	subpage_upper_right_latitude   = 5.764877,
	subpage_upper_right_longitude  = -31.892998,
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
	contour                        = 'off',
	contour_highlight              = 'off',
	contour_hilo                   = 'off',
	contour_label                  = 'off',
	contour_level_list             = [0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 15.0, 20.0, 25.0, 30.0, 40.0, 50.0, 75.0, 100.0, 200.0, 300.0],
	contour_level_selection_type   = 'level_list',
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['rgb(0.95,0.65,0.95)', 'rgb(0.85,0.55,0.85)', 'rgb(0.75,0.5,0.85)', 'rgb(0.6,0.6,1)', 'rgb(0,0.6,1)', 'rgb(0,0.8,1.0)', 'blue_green', 'bluish_green', 'yellow_green', 'greenish_yellow', 'rgb(1,1,0.5)', 'yellow', 'orange_yellow', 'yellowish_orange', 'rgb(1,0.45,0)', 'red', 'rgb(0.8,0,0)', 'burgundy'],
	contour_shade_colour_method    = 'list',
	contour_shade_method           = 'area_fill',
	contour_shade_min_level        = 0.5,
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">Total precipitation (Range: 05 .. 300)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour11',output,area,data,background,contour,foreground,title)
tomv4('contour11',contour)
tohtml('contour11',data,contour)