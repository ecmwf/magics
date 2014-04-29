# importing Magics module

from Magics.macro import *

ref = 'logarithmic'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off', output_name=ref)

# Setting the cartesian view

projection = mmap(
    subpage_y_position=2.,
    subpage_x_position=3.,
    subpage_map_projection='cartesian',
    subpage_x_axis_type='geoline',
    subpage_y_axis_type='logarithmic',
    subpage_x_min_latitude=-20.,
    subpage_x_max_latitude=20.,
    subpage_x_min_longitude=-180.,
    subpage_x_max_longitude=180.,
    subpage_y_min=1.,
    subpage_y_max=50000000.,
    )

# Vertical axis

vertical = maxis(
    axis_orientation='vertical',
    axis_grid='on',
    axis_type = 'position_list',
    axis_tick_label_height=0.4,
    axis_tick_label_colour='navy',
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_reference_level=0.,
    axis_grid_reference_line_style='solid',
    axis_grid_reference_thickness=1,
    axis_grid_line_style='dash',
    axis_title='on',
    axis_tick_position_list = [1., 100., 500., 10000.],
    axis_title_text='My Title',
    axis_title_font='times',
    axis_title_font_style='italic',
    axis_title_height=1.,
    )

# Horizontal axis

horizontal = maxis(
    axis_orientation='horizontal',
    axis_type='geoline',
    axis_tick_label_height=0.4,
    axis_tick_label_colour='navy',
    axis_grid='on',
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dash',
    )

lines = ['Using axis_tick_position_list to taylor the tick on logaritmic axis']

title = mtext({
    'text_lines': lines,
    'text_html': 'true',
    'text_justification': 'left',
    'text_font_size': 1.,
    'text_colour': 'charcoal',
    })

# To the plot
plot(output, projection, vertical, horizontal, title)

#For documentation only
tofortran(ref, output, projection, vertical,horizontal, title)
tomv4(ref, projection, vertical, horizontal)
tohtml(ref, projection, vertical, horizontal)

