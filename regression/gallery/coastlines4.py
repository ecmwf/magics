#importing Magics module
from Magics.macro import *


ref = 'coastlines4'
#Setting of the output file name
output = output(output_formats = ['png'],output_name_first_page_number = "off",output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'cylindrical',
		  subpage_lower_left_latitude= 34.,
		  subpage_lower_left_longitude= -20.,
		  subpage_upper_right_latitude= 60.,
		  subpage_upper_right_longitude= 16.)

#Coastlines setting
coast = mcoast( map_grid = "off",
		map_coastline_resolution = "high",
		map_coastline_land_shade  =  "off",
		map_coastline_colour = "sky",
		map_coastline_sea_shade = "on",
		map_coastline_sea_shade_colour = "sky",
		map_rivers = "on",
		map_rivers_colour = "sky")

title = mtext(
	  text_lines = ["<font size='1'>Sea, lakes and rivers</font>"],
	  text_justification = "left",
	  text_font_size = 0.8,
	  text_colour = "charcoal")

#To the plot
plot(output, projection, coast,title)

#For documentation only
tofortran(ref, output, projection, coast, title)
tomv4(ref,coast)
tohtml(ref,coast)













