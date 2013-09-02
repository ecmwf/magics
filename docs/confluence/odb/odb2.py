
# importing Magics module


###############################################################################
# IMPORTANT : before running you have to set-up your LD_LIBRARY_PATH
# to make sure it content /usr/local/apps/odb_api/0.9.26/lib
# setenv LD_LIBRARY_PATH /usr/local/apps/odb_api/0.9.26/lib:LD_LIBRARY_PATH
###############################################################################

from Magics.macro import *
import numpy as np
import sys, os
sys.path.insert(0, '/usr/local/apps/odb_api/0.9.26/lib/python2.7.1/site-packages')
import odb



odb = np.array([r[:] for r in odb.sql( "select lat,lon,obsvalue from '%s'" % 'data.odb')])


lat = odb[:,0]
lon = odb[:,1]
obsvalue = odb[:,2] - 273.15



ref = 'odb2'

# Setting of the output file name
output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)


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

lines = ['ODB : using numpy arrays ..', 
		'<font size ="0.8i"> Here the values (temperature) have been transformed from Kelvin to Celsius </font>' ]

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

tohtml(ref, input, symbol)
