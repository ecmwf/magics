# importing Magics module

from Magics.macro import *

ref = 'ecchart'

# Setting of the output file name

output = output(output_formats=['ps'],
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


# Define a contour 
ecchart = mcont(contour_automatic_setting='ecchart',)

data = []
for i in range(1,13) :
	data.append(coast)
	data.append(mgrib(grib_input_file_name='data.grb', grib_field_position=i))
	data.append(ecchart)
	data.append(mtext())
	data.append(page())


# Define a contour 

# To the plot

plot(
    output,
	data
    )

