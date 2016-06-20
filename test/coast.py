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

ref = 'coast'

# Setting of the output file name

output = output(output_formats=['ps'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

globe = mmap(
    subpage_map_projection='cylindrical',
    )

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_colour='tan',
               )


title = \
    mtext(text_lines=["<font size='1'>BASIC OUTPUT</font>"
          , '',
          ], text_justification='left', text_font_size=0.5,
          text_colour='charcoal')

# To the plot

plot( output,  globe, coast, )
tofortran(ref, output,  globe, coast, )


