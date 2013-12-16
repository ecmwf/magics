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


