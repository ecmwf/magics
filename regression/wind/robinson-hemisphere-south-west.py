# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module

from Magics.macro import *

# Setting the coordinates of the geographical area
south = {
    'subpage_map_projection':'polar_stereographic',
    'subpage_map_hemisphere':'south',
    'subpage_lower_left_latitude':-3.,
    'subpage_lower_left_longitude':60.,
    'subpage_upper_right_latitude':-30.,
    'subpage_upper_right_longitude':-100.,
    }

north = {
    'subpage_map_projection':'polar_stereographic',
    'subpage_map_hemisphere':'north',
    }

mollweide = {
     'subpage_map_projection':'mollweide',
     'subpage_map_hemisphere':'mollweide',
        }
polar_north = {
     'subpage_map_projection':'polar_north',
     'subpage_map_hemisphere':'polar_north',
        }

robinson = {
     'subpage_map_projection':'robinson',
     'subpage_map_hemisphere':'robinson',
     }

# Coastlines setting


# Example reference

for i in ["south-west"]:
    for area in [robinson]:
        ref = '%s-hemisphere-%s' % (area["subpage_map_hemisphere"], i)


# Setting of the output file name

        png = output(output_formats=['png'],
                        output_name_first_page_number='off',
                        output_name=ref)

        coast = mcoast(map_grid='on', map_grid_colour='tan',
                       map_coastline_colour='tan',
                       )

# Import andthe  data


        obs = mobs(obs_input_file_name='bufr-synop-%s-winds.bufr'%i)



        title = \
            mtext(text_lines=["<font size='1'>Observation Plotting [Synop]: %s winds </font>" % i
                  , '',
                  ], text_justification='left', text_font_size=0.5,
                  text_colour='charcoal')



# To the plot
        plot( png,  mmap(area), obs, coast, title)

