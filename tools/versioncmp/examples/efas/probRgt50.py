# -*- coding: utf-8 -*-
# USAGE:
# python probRgt50.py

# importing Magics module
from Magics.macro import *

ref = 'probRgt50'

# Setting of the output file name

output = output(output_formats=[ 'png'],
				output_name_first_page_number='off',
                output_name=ref, output_width=680 )
                


width = 20.
height =  (810*width)/680



# Setting the coordinates of the geographical area to be adapted tto your requierment
projection = mmap(super_page_x_length=width,
				super_page_y_length = height, 
				subpage_x_position= 0.,
				subpage_y_position= 0.,
				subpage_x_length= width,
				subpage_y_length= height,
				world_file_path = "efas.wld",
				subpage_x_automatic='on',
				subpage_y_automatic='on',
				subpage_x_axis_type='regular',
				subpage_y_axis_type='regular',
                subpage_map_projection='cartesian',
                page_id_line='off',
                subpage_frame='off')

efas = mgrib(grib_input_file_name='probRgt50.grb',grib_field_position=1)
            

# Define  contour for probRgt50 --> Found the description in your doc 
contour = mcont(contour='off', contour_label='off', contour_shade='on', 
		contour_level_selection_type="level_list",
		contour_level_list=[0.,14.,28.,42.,56.,70.,84.,98.,112.,126.,1400.],
		contour_shade_technique = "dump_shading",
		contour_shade_colour_method = "list",
		contour_shade_colour_list = ["RGB(0.8,0.98,0.4)", "RGB(0.57,0.93,0.26)", 
			"RGB(0.34,0.9,0.12)","RGB(0.24,0.82,0.21)","RGB(0.24, 0.72,0.39)",
			"RGB(0.19, 0.63, 0.56)", "RGB(0.12, 0.52,0.65)", "RGB(0.13,0.34,0.58)",
			"RGB(0.11,0.2,0.53)", "RGB(0.05,0.06,0.47)"],
		legend='off', legend_display_type='continuous'
	)


# To the plot
plot(
    output, 
	projection, efas, contour,
    )


  
