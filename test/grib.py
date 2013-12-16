# importing Magics module

from Magics.macro import *


# Example reference

ref = 'grib'

# Setting of the output file name

output = output(output_formats=['ps'],
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

# Import the z500 data

data = mgrib(grib_input_file_name='data.grib',
             grib_id='data')



cont = mcont()

title = \
    mtext(text_lines=["<font size='1'>Grib Field</font>"
          , '',
          "<grib_info id='data' key='name' format='%s'/> <grib_info id='data' key='valid-date'/>"
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
plot( output,  europe, data, cont, coast, )
tofortran(ref, output,  europe, data, cont, coast, )


