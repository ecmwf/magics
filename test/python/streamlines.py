# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *

#Example reference
ref = 'streamlines'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
australia = mmap(subpage_upper_right_longitude= 180.,
                 subpage_upper_right_latitude= -5.,
                 subpage_lower_left_longitude= 100.,
                 subpage_map_projection= 'cylindrical',
                 subpage_lower_left_latitude= -55.)

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'off',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'off',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast(  map_grid= 'on',
		      map_grid_colour = 'tan',
		      map_label= 'off',
		      map_coastline_colour= 'tan',
		      map_coastline_land_shade= 'off',
                      map_coastline_sea_shade= 'off')

#Import the  wind  at 200hPa uv200 
uv200 =  mgrib( grib_input_file_name = './uv200.grb',grib_id= 'uv200')

uv200_wind = mwind(
                legend= 'on',
                wind_field_type = 'streamlines',
		wind_arrow_unit_velocity = 40.0,
		wind_arrow_min_speed = 20.0,                
		wind_thinning_factor= 3.,
		wind_advanced_method = 'on',
		wind_advanced_colour_selection_type = 'interval',
		wind_advanced_colour_level_interval = 10.0,
		wind_advanced_colour_reference_level = 10.0,
		wind_advanced_colour_max_value = 100.0,
		wind_advanced_colour_min_value = 20.0,
		wind_advanced_colour_table_colour_method = 'calculate',
		wind_advanced_colour_direction = 'anti_clockwise',
		wind_advanced_colour_min_level_colour = 'turquoise',
		wind_advanced_colour_max_level_colour = 'purple_red')

title = mtext( text_lines = ["<font size='1'>Wind arrow colour is a function of speed</font>"],
	       text_justification = 'left',
	       text_font_size = 0.5,
	       text_colour = 'charcoal')

#add a legend
legend = mlegend(legend= 'on',
           legend_text_colour= 'charcoal',
           legend_box_mode= 'positional',
           legend_box_x_position= 27.5,
           legend_box_y_position= 4.,
           legend_box_x_length= 2.,
           legend_box_y_length= 12.,
           legend_border= 'off',
           legend_border_colour= 'charcoal',
           legend_box_blanking= 'on',
           legend_display_type= 'continuous',
           legend_title = 'on',
	   legend_title_text= 'Wind speed at 200 hPa',
	   legend_text_font_size = '0.5')

#To the plot
plot(output, australia, background,uv200, uv200_wind,foreground, title, legend)

#For documentation only
tofortran(ref, output, australia, background,uv200, uv200_wind,foreground, title, legend)
tomv4(ref,uv200_wind)
tohtml(ref,uv200_wind)
