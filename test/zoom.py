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

ref = 'zoom'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

europe = mmap(
    subpage_upper_right_longitude=20.,
    isubpage_map_projection='cylindrical',
    subpage_map_projection='polar_stereographic',
    subpage_lower_left_longitude=-20.,
    subpage_lower_left_latitude=40.,
    subpage_upper_right_latitude=60.,
    )

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_colour='tan',
               )

# Import andthe  data

obs = mobs(obs_input_file_name='synop.bufr')




title = \
    mtext(text_lines=["<font size='1'>Observation Plotting [Synop] </font>"
          , '',
          ], text_justification='left', text_font_size=0.5,
          text_colour='charcoal')



# To the plot

print "plot"
plot( output,  europe, obs, coast, )
tofortran(ref, output,  europe, obs, coast, )


