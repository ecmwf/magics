# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# importing Magics module

from Magics.macro import *
import numpy as np
import sys, os
sys.path.insert(0, '/usr/local/lib/metaps/lib/odalib/0.9.8/python2.7/site-packages/')
import odb


data = 'data.odb'

odb = np.array([r[:] for r in odb.sql( "select lat,lon,obsvalue from '%s'" % data)])


print odb 

lat = odb[:,0]
lon = odb[:,1]
obsvalue = odb[:,2] - 273.15



ref = 'odb'

# Setting of the output file name
output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area
# Here we use Europe
area = mmap(subpage_lower_left_llonslonsatitude=40.,
            subpage_lower_left_longitude=-20.,
            subpage_upper_right_latitude=65.,
            subpage_upper_right_longitude=10.)

# Background Coastlines

background = mcoast(
    map_coastline_sea_shade_colour='white',
    map_coastline_land_shade_colour='cream',
    map_grid='on',
    map_coastline_land_shade='on',
    map_coastline_sea_shade='on',
    map_label='on',
    map_coastline_colour='tan',
    )

# Import the input data

input = minput(input_latitude_values=lat,
               input_longitude_values=lon, 
			   input_values = obsvalue)

# Define the symbol plotting

symbol = msymb(legend='on', symbol_type='marker',
               symbol_colour='red', 
			   symbol_advanced_table_selection_type='interval',
		       symbol_advanced_table_interval=5.,
		       symbol_advanced_table_min_level_colour='blue',
		       symbol_advanced_table_max_level_colour='red',
		       symbol_advanced_table_colour_direction='clockwise',
               symbol_table_mode='advanced',
			   symbol_marker_index=15)

lines = ['ODB First example...']

title = mtext(
    text_lines= lines,
    text_html= 'true',
    text_justification= 'left',
    text_font_size= 1.,
    text_colour= 'charcoal',
	text_mode='positional',
	text_box_y_position=16.5
    )

legend = mlegend(legend='on', 
				legend_text_colour='navy',
                legend_display_type='histogram')

# To the plot

plot(
    output,
    #area,
    background,
    input,
    symbol,
    title,
	legend
    )
