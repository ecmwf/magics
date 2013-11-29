
#importing Magics module
from Magics.macro import *

#Example reference
ref = 'wind2'

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

#Import the  wind speed at 200hPa speed200 
speed200 =  mgrib( grib_input_file_name = './speed200.grb', grib_id= 't850')

#Define the shading for the wind speed
speed200_contour = mcont(
    legend= 'on',
    contour_level_selection_type= 'level_list', 
    contour_level_list= [30., 40., 50., 60., 70., 80., 90., 100.], 
    contour_shade= 'on', 
    contour_shade_min_level_colour= 'blue_green', 
    contour_shade_max_level_colour= 'blue_purple',
    contour_shade_method= 'area_fill', 
    contour_reference_level= 0., 
    contour_highlight= 'off', 
    contour_hilo= 'off', 
    contour_hilo_format= '(F3.0)', 
    contour_hilo_height= 0.2, 
    contour_hilo_type= 'number', 
    contour_hilo_suppress_radius= 30., 
    contour_hi_min_value= 15., 
    contour_label= 'off')


#Import the  wind  at 200hPa uv200 
uv200 =  mgrib( grib_input_file_name = './uv200.grb',grib_id= 'uv200')

uv200_wind = mwind(
                legend= 'on',
                wind_field_type = 'flags',
                wind_flag_length = 0.8,
                wind_thinning_factor= 6.,                
		wind_flag_origin_marker = 'circle',
		wind_flag_min_speed = 20.0,
		wind_flag_colour = 'charcoal')

title = mtext( text_lines = ["<font size='1'>Wind flags where speed is over 20 m/s</font>"],
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
plot(output, australia, background,speed200, speed200_contour,uv200, uv200_wind,foreground, title, legend)

#For documentation only
tofortran(ref, output, australia, background,speed200, speed200_contour,uv200, uv200_wind,title, legend)
tomv4(ref,uv200_wind)
tohtml(ref,uv200_wind)
