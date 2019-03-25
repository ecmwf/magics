# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='wind_gust.grib',grib_field_position=1)

#Setting output
output = output(
	output_formats                = ['png'],
	output_name                   = 'contour14',
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude    = 24.044245,
	subpage_lower_left_longitude   = -125.233253,
	subpage_map_projection         = 'cylindrical',
	subpage_upper_right_latitude   = 49.888611,
	subpage_upper_right_longitude  = -66.454811,
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
	contour_label                  = 'off',
	contour_level_list             = [10.0, 15.0, 20.0, 25.0, 30.0, 40.0, 50.0, 60.0, 80.0, 100.0],
	contour_level_selection_type   = 'list',
	contour_line_thickness         = 1,
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['rgb(1,1,0.40)', 'greenish_yellow', 'rgb(0.58,1,0.0)', 'avocado', 'evergreen', 'rgb(0.0,0.33,0.28)', 'rgb(0.0,0.26,0.28)', 'rgb(0.0,0.20,0.44)', 'rgb(0.0,0.20,0.64)'],
	contour_shade_colour_method    = 'list',
	contour_shade_method           = 'area_fill',
	contour_shade_min_level        = 10.0,
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">wind gust - Contour shade (Range: 10 .. 100)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour14',output,area,data,background,contour,foreground,title)
tomv4('contour14',contour)
tohtml('contour14',data,contour)