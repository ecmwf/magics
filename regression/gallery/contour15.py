from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='CAPE.grib',grib_field_position=1)

#Setting output
output = output(
	output_formats                = ['png'],
	output_name                   = 'contour15',
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude    = -90,
	subpage_lower_left_longitude   = -180,
	subpage_map_projection         = 'cylindrical',
	subpage_upper_right_latitude   = 90,
	subpage_upper_right_longitude  = 180,
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
	contour_hilo                   = 'off',
	contour_interval               = 200.0,
	contour_label                  = 'off',
	contour_level_list             = [100.0, 200.0, 300.0, 500.0, 750.0, 1000.0, 1250.0, 1500.0, 1750.0, 2000.0, 2250.0, 2500.0, 2750.0, 3000.0, 3500.0, 4000.0, 4500.0],
	contour_level_selection_type   = 'level_list',
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['rgb(1.00,0.35,0.00),rgb(1.00,  0.55,  0.00)', 'rgb(1.00,  0.67,  0.00)', 'rgb(1.00,  0.78,  0.00)', 'rgb(1.00,  0.89,  0.00)', 'rgb(1.00,  1.00,  0.00)', 'rgb(0.75,  0.98,  0.00)', 'rgb(0.60,  0.86,  0.10)', 'rgb(0.43,  0.77,  0.20)', 'rgb(0.30,  0.68,  0.40)', 'rgb(0.50,  0.90,  1.00)', 'rgb(0.50,  0.70,  1.00)', 'rgb(0.37,  0.50,  1.00)', 'rgb(0.20,  0.30,  0.90)', 'rgb(0.10,  0.15,  0.75)', 'rgb(0.00,  0.05,  0.55)', 'rgb(0.00,  0.00,  0.35)', 'rgb(0.00,  0.00,  0.20)'],
	contour_shade_colour_method    = 'calculate',
	contour_shade_label_blanking   = 'on',
	contour_shade_max_level        = 4200.0,
	contour_shade_method           = 'area_fill',
	contour_shade_min_level        = 100.0,
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">CAPE - Shade from 100 to 4500 (orange-blue)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour15',output,area,data,background,contour,foreground,title)
tomv4('contour15',contour)
tohtml('contour15',data,contour)