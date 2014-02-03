
#importing Magics module
from Magics.macro import *

#Example reference
ref = 'geowind'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
map = mmap(subpage_upper_right_longitude= 59.29,
                 subpage_upper_right_latitude= 53.63,
                 subpage_lower_left_longitude= -27.23,
                 subpage_map_projection= 'polar_stereographic',
                 subpage_lower_left_latitude= 10.49)

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'off',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'off',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast( 
                 map_grid_colour = 'tan',
                 map_coastline_colour= 'tan',
                 map_coastline_land_shade= 'off',
                 map_coastline_sea_shade= 'off')


#Import the  wind speed at 200hPa speed200 
data =  mgeo( geo_input_file_name = './testwind.geo',
               )



wind = mwind(
                legend= 'on',
                wind_field_type= 'arrows',
                wind_arrow_thickness= 2,
                wind_arrow_colour= 'evergreen' )


title = mtext(text_lines = ["<font size='1'>Geopoint inputs/font>",],
	      text_justification = 'left',
	      text_font_size = 0.5,
	      text_colour = 'charcoal')



#To the plot
#plot(output, map, foreground, wind, all, wind, thinning, title)

plot(output, map,  foreground, data, wind, title)
