from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='700_hPa_relative_humidity.grib',grib_field_position=1)

#Setting output
output = output(
	output_formats                = ['png'],
	output_name                   = 'contour17',
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude    = -44.14397,
	subpage_lower_left_longitude   = 112.411057,
	subpage_map_projection         = 'cylindrical',
	subpage_upper_right_latitude   = -9.562805,
	subpage_upper_right_longitude  = 154.139252,
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
	contour_level_list             = [65.0, 80.0, 95.0, 200.0],
	contour_level_selection_type   = 'level_list',
	contour_line_thickness         = 1,
	contour_min_level              = 65.0,
	contour_reference_level        = 65.0,
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['rgb(0.8,1,0.8)', 'rgb(0.7,0.95,0.9)', 'rgb(0.6,0.75,1)'],
	contour_shade_colour_method    = 'list',
	contour_shade_method           = 'area_fill',
	contour_shade_min_level        = 65.0,
	contour_shade_min_level_colour = 'yellowish_green',
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">700 hPa relative humidity (Range: 65 .. 100)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour17',output,area,data,background,contour,foreground,title)
tomv4('contour17',contour)
tohtml('contour17',data,contour)