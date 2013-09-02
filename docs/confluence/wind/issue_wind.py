
#importing Magics module
from Magics.macro import *

#Example reference
ref = 'issue_wind'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
#australia = mmap(subpage_upper_right_longitude= 180.,
#                 subpage_upper_right_latitude= -5.,
#                 subpage_lower_left_longitude= 100.,
#                 subpage_lower_left_latitude= -55.,
		 #subpage_map_projection= 'cylindrical')

globe = mmap(    subpage_upper_right_longitude= 120.,
                 subpage_upper_right_latitude=  -20.,
                 subpage_lower_left_longitude= 40.,
                 subpage_lower_left_latitude= -70.,
                 subpage_map_projection= 'cylindrical')

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
		      map_coastline_colour= 'tan')

#Import the  wind  at 200hPa uv200 
uv700 =  mgrib( grib_input_file_name = './wind.grib',grib_id= 'uv700')


uv700_wind = mwind(
    wind_advanced_method = 'on',
    wind_advanced_colour_max_level_colour = 'red',
    wind_advanced_colour_min_level_colour = 'cream'
)

title = mtext( text_lines = ['<font size="1">Wind arrow colour is a function of speed</font>'],
	       text_justification = 'left',
	       text_font_size = 0.5,
	       text_colour = 'charcoal')

#To the plot
plot(output, globe, background,uv700, uv700_wind,foreground, title)
