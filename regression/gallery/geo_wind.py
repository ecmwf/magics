# importing Magics module

from Magics.macro import *

ref = 'geo_wind'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area
# Here we use Europe

area = mmap(subpage_lower_left_latitude=40.,
            subpage_lower_left_longitude=-20.,
            subpage_upper_right_latitude=65.,
            subpage_upper_right_longitude=10.)

# Background Coastlines

background = mcoast(
    map_coastline_sea_shade_colour='white',
    map_coastline_land_shade_colour='cream',
    map_grid='on',
    map_coastline_land_shade='on',
    map_coastline_sea_shade='on',
    map_label='on',
    map_coastline_colour='tan',
    )

# Import the input data

input = minput(input_latitude_values = [42., 47., 52.],
		input_longitude_values = [2., -2., -10.],
		input_x_component_values = [ 50., 0., -30.],
		input_y_component_values = [ 0., 20., 0.])


# Define the symbol plotting

symbol = msymb(legend='on', symbol_type='wind',
			   wind_advanced_method = 'on',)

lines = ['Using wind as Symbol ...']

title = mtext(
    text_lines= lines,
    text_html= 'true',
    text_justification= 'left',
    text_font_size= 1.,
    text_colour= 'charcoal',
    )
    
legend = mlegend(
    legend='on',
    legend_title='on',
    legend_title_text="<font colour='navy' size='0.5'> using advanced mode  </font>",
    legend_display_type='continuous',
    
    )
# To the plot

plot(
    output,
    area,
    background,
    input,
    symbol,
    legend,
    title,
    )
#For documentation only
tofortran(ref, output,
    area,
    background,
    input,
    symbol,
    legend,
    title,)
tomv4(ref, input)
tohtml(ref, input, symbol)
