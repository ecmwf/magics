# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module

from Magics.macro import *


# Example reference

ref = 'cairo'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

europe = mmap(
    subpage_upper_right_longitude=60.,
    subpage_map_projection='cylindrical',
    subpage_lower_left_longitude=-20.,
    subpage_lower_left_latitude=30.,
    subpage_upper_right_latitude=60.,
    )

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_colour='tan',
               )


title = \
    mtext(text_lines=["<font size='1'>PNG OUTPUT</font>"
          , '',
          ], text_justification='left', text_font_size=0.5,
          text_colour='charcoal')

# add a legend

legend = mlegend(
    legend='on',
    legend_text_colour='black',
    legend_box_mode='positional',
    legend_box_x_position=19.0,
    legend_box_y_position=1.,
    legend_box_x_length=2.,
    legend_box_y_length=14.,
    legend_display_type='continuous',
    legend_title='on',
    legend_title_text='Temperature',
    legend_text_font_size='0.4',
    )

# To the plot

print "plot"
plot( output,  europe, coast, )
tofortran(ref, output,  europe, coast, )


