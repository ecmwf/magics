# importing Magics module

from Magics.macro import *

ref = 'rotated'

# Setting of the output file name

output = output(output_formats=['ps'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

projection = mmap(subpage_map_projection='cylindrical',
                  subpage_lower_left_latitude=40.,
                  subpage_lower_left_longitude=-10.,
                  subpage_upper_right_latitude=70.,
                  subpage_upper_right_longitude=20.)

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='on',
               map_coastline_land_shade_colour='cream',
               map_coastline_colour='tan')

# Import the z500 data

z = mgrib(grib_input_file_name='z.grb')
wind = mgrib(grib_input_file_name='wind.grb')

# Define the simple contouring for z500

contour = mcont()
    
arrows = mwind(wind_thinning_factor = 3.00 )

title = \
    mtext(text_lines=["<font size='1'>Rotated grids</font>"], 
          text_justification='left', 
          text_font_size=0.8,
          text_colour='charcoal')

# To the plot

plot(
    output,
    projection,
    coast,
    z,
    contour,
    wind, arrows,
    title,
    )

