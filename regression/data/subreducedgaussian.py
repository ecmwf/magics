# importing Magics module

from Magics.macro import *

ref = 'subreducedgaussian'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

projection = mmap(subpage_map_projection='cylindrical',
                subpage_lower_left_latitude= 0.,
                subpage_lower_left_longitude= -70.,
                subpage_upper_right_longitude= 70.,
                subpage_upper_right_latitude= 90.,
                page_id_line = 'off'
                )

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='on',
               map_coastline_land_shade_colour='cream',
               map_coastline_colour='tan')

# Import the z500 data

data = mgrib(grib_input_file_name='subreducedgaussian.grib')

# Define the simple contouring for z500

contour = mcont(
    legend='off',
    contour_line_colour='navy',
    contour_line_thickness=2,
    contour_label='on',
    contour_highlight_colour='navy',
    contour_highlight_thickness=6,
    )

title = \
    mtext(text_lines=["<font size='1'>Reduced Gaussain Grid...</font>"
          ,
          "<font colour='evergreen'>only a subarae</font> "
          ,
          ], text_justification='left', text_font_size=0.8,
          text_colour='charcoal')

# To the plot

plot(
    output,
    projection,
    coast,
    data,
    contour,
    mtext(),
    )

