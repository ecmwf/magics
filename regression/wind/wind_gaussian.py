
#importing Magics module
from Magics.macro import *

#Example reference
ref = 'wind_gaussian'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
polar = mmap(subpage_upper_right_longitude= 11.13,
                 subpage_upper_right_latitude= 64.,
                 subpage_lower_left_longitude= -12.,
                 subpage_map_projection= 'polar_stereographic',
                 subpage_lower_left_latitude= 48.11)
polar = mmap(subpage_upper_right_longitude= 2.5,
                 subpage_upper_right_latitude= 62.,
                 subpage_lower_left_longitude= -2.,
                 subpage_map_projection= 'polar_stereographic',
                 subpage_lower_left_latitude= 58.11)
#Setting the coordinates of the geographical area
area = mmap(subpage_upper_right_longitude= 35.,
                 subpage_upper_right_latitude= 80.,
                 subpage_lower_left_longitude= 0.,
                 subpage_lower_left_latitude= 60.)

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'off',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'off',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast( map_grid= 'on',
                 map_grid_colour = 'tan',
                 map_label= 'off',
                 map_coastline_colour= 'tan',
                 map_coastline_land_shade= 'off',
                 map_coastline_sea_shade= 'off')

#Import the z500 data 
data =  mgrib( grib_input_file_name = 'wind_reduced_gg.grib',
                grib_id= 'z500')

wind = mwind(
                legend= 'off',
                wind_field_type= 'flags',
                wind_flag_length    = 0.7,
                wind_flag_colour             = "ORANGISH_RED",
                wind_flag_origin_marker_size = 0.1,
               wind_thinning_factor         = 8.,
               wind_thinning_debug = "off")

title = mtext(text_lines = ["<font size='1'>Wind arrows with legend</font>",
	                    "",
	                    "<grib_info id='z500' key='name' format='%s'/>"],
	      text_justification = 'left',
	      text_font_size = 0.5,
	      text_colour = 'charcoal')

#add a legend
legend = mlegend(legend= 'on',
           legend_text_colour= 'black',
           legend_box_mode= 'positional',
           legend_box_x_position= 15.,
           legend_box_y_position= 0.5,
           legend_box_x_length= 4.,
           legend_box_y_length= 18.,
           legend_border= 'off',
           legend_border_colour= 'black',
           legend_box_blanking= 'on',
           legend_display_type= 'continuous',
           legend_title = "on",
	   legend_title_text= "Wind",
	   legend_text_font_size = "0.5")

#To the plot
plot(output, area, background, data, wind,foreground, title, legend)
#plot(output, area, background, data, wind, speed, contour, foreground, title, legend)

