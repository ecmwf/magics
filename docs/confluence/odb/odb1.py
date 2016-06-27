# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# importing Magics module

from Magics.macro import *


ref = 'odb1'

# Setting of the output file name
output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area
# Here we use Europe
area = mmap(subpage_lower_left_latitude=40.,
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

# Import odb data
odb = odb_geopoints(odb_filename= "data.odb",
          odb_latitude_variable='lat@hdr',
          odb_longitude_variable= 'lon@hdr',
          odb_value_variable='obsvalue@body'
    )

# Define the symbol plotting
symbol = msymb(symbol_type='marker',
               symbol_colour='navy', 
			   symbol_marker_index=15)

lines = ['Using odb filter and odb geopoints...', "<magics_title/>"]

title = mtext(
    text_lines= lines,
    text_html= 'true',
    text_justification= 'left',
    text_font_size= 0.7,
    text_colour= 'charcoal',
	text_mode='positional',
	text_box_y_position=17.5
    )

# To the plot

plot(
    output,
    area,
    background,
    odb,
    symbol,
    title,
    )
tohtml(
	ref, 
    odb,
    symbol,
	)
