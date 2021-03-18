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
	output_name                   = 'contour13',
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
	contour_highlight              = 'off',
	contour_hilo                   = 'off',
	contour_label                  = 'off',
	contour_level_list             = [0.3, 1.6, 3.4, 5.5, 8.0, 10.8, 13.9, 17.2, 20.8, 24.5, 28.5, 32.7, 50.0],
	contour_level_selection_type   = 'level_list',
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['rgb(0.85,1,1)', 'rgb(0.65,1,1)', 'rgb(0.45,0.95,1)', 'rgb(0.6,1,0.6)', 'rgb(0.65,0.95,0.2)', 'rgb(1,1,0)', 'rgb(1,0.85,0)', 'rgb(1,0.7,0)', 'rgb(1,0.5,0)', 'rgb(1,0,0)', 'rgb(0.9,0,0.5)', 'rgb(0.7,0,0.7)'],
	contour_shade_colour_method    = 'list',
	contour_shade_method           = 'area_fill',
	contour_shade_min_level        = 0.0,
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">wind gust - Contour shade (Beaufort wind scale)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour13',output,area,data,background,contour,foreground,title)
tomv4('contour13',contour)
tohtml('contour13',data,contour)