# importing Magics module

from Magics.macro import *

ref = 'contour8'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

projection = mmap(subpage_upper_right_longitude=180.00,
                  subpage_upper_right_latitude=90.00,
                  subpage_lower_left_latitude=-90.00,
                  subpage_lower_left_longitude=-180.0,
                  subpage_map_projection='cylindrical')



coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='on',
               map_coastline_land_shade_colour='cream',
               map_coastline_colour='tan')

# Import the data
data = mgrib(grib_input_file_name='Total_precipitation.grib')

# Define a contour 
ecchart = mcont(contour_automatic_setting='ecchart',)



# Define a title 
title = mtext(
           text_lines = ["<font size='1'>Choose eccharts styling depending on the parameter</font>",
					"Here: ecchart for parameter <grib_info key='name'/> at  <grib_info key='level'/> hPa",
		   			"<font colour='evergreen'>contour_automatic_setting=ecchart</font> ",],
		   text_justification = "left",
		   text_font_size = 0.8,
           text_colour =  "charcoal")

# To the plot

plot(
    output, projection, coast,
	data, ecchart, title)

#For documentation only
tofortran(ref, output,  projection, coast, data, ecchart, title)
tomv4(ref, ecchart)
tohtml(ref, ecchart)

