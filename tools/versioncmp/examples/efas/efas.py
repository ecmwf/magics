# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# -*- coding: utf-8 -*-
# USAGE:

# importing Magics module
from Magics.macro import *

ref = 'rivers'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name=ref, 
				output_name_first_page_number='off',
				output_cairo_antialias='on', 
				output_width=680 )
                

width = 20.
height =  (810*width)/680



# Setting the coordinates of the geographical area to be adapted tto your requierment
projection = mmap(super_page_x_length=width,
				super_page_y_length = height, 
				subpage_x_position= 0.,
				subpage_y_position= 0.,
				subpage_x_length= width,
				subpage_y_length= height,
				subpage_x_axis_type='regular',
				subpage_y_axis_type='regular',
				subpage_x_automatic="on",
				subpage_y_automatic="on",
				world_file_path = "efas.wld",
                subpage_map_projection='cartesian',
                page_id_line='off',
                subpage_frame='off')

# Coastlines setting
coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='off',
               map_coastline_land_shade_colour='cream',
               map_coastline_resolution='high',
               map_coastline_colour='tan')

efas = mgrib(grib_input_file_name='EUE2013011712_summary.grb',grib_field_position=15)

# Define  contour for probRgt50 --> Found the description in your doc 
contour = mcont(contour='off', contour_label='off', contour_shade='on', 
		contour_shade_technique = "dump_shading",
		contour_shade_max_level_colour = "red",
		contour_shade_min_level_colour = "blue",
		contour_shade_colour_direction = "clockwise",
		legend='off', legend_display_type='continuous'
	)


title = mtext(text_lines=["<font size='1'>Efas Grib File ... probRgt50</font>"],
          text_justification='left', text_font_size=0.8,
          text_colour='charcoal')

# To the plot
plot( 
    output, 
	projection, efas, contour, 
    )

# Questions:
# We have to start from this file: /gpfs/r_lxab/efas/ma9/pcr2grb_test_output/EUE2013011712/EUE2013011712_summary.grb.
# Is there a way to read it and select the GRIB message probRgt50?
# How to switch off the ECMWF logo and the other footer (lower left)?

  
