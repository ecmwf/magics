
# importing Magics module
from Magics.macro import *

ref = 'era'

# Setting of the output file name

output = output(output_formats=['ps ', 'png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

projection = mmap(
    subpage_x_length=24.,
    subpage_upper_right_longitude=50.00,
    subpage_upper_right_latitude=65.00,
    subpage_lower_left_latitude=25.00,
    subpage_lower_left_longitude=-20.0,
    subpage_map_projection='cylindrical',
    )

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='grey',
               map_grid_thickness=2,
               map_coastline_colour='RGB(0.4,0.4,0.4)',
               map_coastline_thickness=3)


obs = mobs(
	obsjson_input_filename = "obs.json",
	obs_template_file_name = "obs.template",
	obs_distance_apart = 0.
	)

title = mtext(text_lines=["Observation plotting ..."  ], 
		text_justification='left', text_font_size=0.8,
         text_colour='charcoal')


# To the plot
plot(
    output,
    projection,
	obs,
    coast,
    title,
    )
