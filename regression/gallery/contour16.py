# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from Magics.macro import *

#Loading GRIB file
data = mgrib(grib_input_file_name='700_hPa_divergence.grib',grib_field_position=1)

#Setting output
output = output(
	output_formats                = ['png'],
	output_name                   = 'contour16',
	output_name_first_page_number = 'off')

#Setting the geographical area
area = mmap(
	subpage_lower_left_latitude    = 15.275416,
	subpage_lower_left_longitude   = 73.057693,
	subpage_map_projection         = 'cylindrical',
	subpage_upper_right_latitude   = 54.06086,
	subpage_upper_right_longitude  = 135.273911,
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
	contour_level_list             = [-200.0, -100.0, -50.0, -20.0, -15.0, -11.0, -7.0, -5.0, -3.0, -1.0, 1.0, 3.0, 5.0, 7.0, 11.0, 15.0, 20.0, 50.0, 100.0, 200.0],
	contour_level_selection_type   = 'level_list',
	contour_shade                  = 'on',
	contour_shade_colour_list      = ['rgb(0,0,0.3)', 'rgb(0,0,0.5)', 'rgb(0,0,0.9)', 'rgb(0,0.3,1)', 'rgb(0,0.45,1)', 'rgb(0,0.75,1)', 'rgb(0.2,0.95,1)', 'rgb(0.45,1,1)', 'rgb(0.75,1,1)', 'none', 'rgb(1,1,0)', 'rgb(1,0.9,0)', 'rgb(1,0.8,0)', 'rgb(1,0.6,0)', 'rgb(1,0.4,0)', 'rgb(1,0.3,0)', 'rgb(0.9,0,0)', 'rgb(0.5,0,0)', 'rgb(0.3,0,0)'],
	contour_shade_colour_method    = 'list',
	contour_shade_method           = 'area_fill',
	legend                         = 'on',
)

#Picking the grib metadata
title = mtext(
    text_lines                     = ['<font size="1">700 hPa divergence (Range: -200 .. 200)</font>','<magics_title/>'],
    text_justification             = 'left',
    text_font_size                 = 0.6,
    text_colour                    = 'charcoal')     

#Plotting
plot(output,area,background,data,title,contour,foreground)

#For documentation only
tofortran('contour16',output,area,data,background,contour,foreground,title)
tomv4('contour16',contour)
tohtml('contour16',data,contour)