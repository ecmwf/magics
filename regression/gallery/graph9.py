# importing Magics module

from Magics.macro import *

ref = 'graph9'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the cartesian view

projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_y_axis_type='regular',
    subpage_x_date_min='2012-03-01 12:00:00',
    subpage_x_date_max='2012-03-07 12:00:00',
    subpage_y_min=30.,
    subpage_y_max=75.,
    )

# Vertical axis

vertical = maxis(
    axis_orientation='vertical',
    axis_type='regular',
    axis_tick_label_height=0.40,
    axis_tick_label_colour='navy',
    axis_grid='on',
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    )

# Horizontal axis

horizontal = maxis(
    axis_orientation='horizontal',
    axis_type='date',
    axis_grid='on',
    axis_days_label_height=0.40,
    axis_months_label_height=0.40,
    axis_years_label_height=0.50,
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    )

# define  the  data

x = ['2012-03-02 00:00:00', '2012-03-02 12:00:00',
     '2012-03-03 00:00:00', '2012-03-04 00:00:00', 
     '2012-03-04 12:00:00',	'2012-03-05 00:00:00', 
     '2012-03-06 12:00:00', '2012-03-07 00:00:00' ]
y1 = numpy.array([35., 45., 40., 45., 40., 30.,35., 45.,])
y2 = y1+5
y3 = y1+10

solid_input = minput(input_date_x_values =x,
	input_y_values=y1)

# Define the graph

solid = mgraph(
    graph_type='bar',
    graph_bar_justification='centre',
    graph_bar_colour='cyan',
    graph_bar_width=3 * 3600.,
    legend='on',
    graph_shade_style = "solid",
    legend_user_text="<font colour='cyan'> solid </font>"
        ,
    )
# define  the  data
dot_input = minput(input_date_x_values =x,
	input_date_x2_values =x,
	input_y_values=y1,
	input_y2_values=y2)
	
# define  the  graph
dot = mgraph(
    graph_type='bar',
    graph_bar_justification='centre',
    graph_bar_colour='green',
    graph_bar_width=3 * 3600.,
    legend='on',
    graph_shade_style = "dot",
    graph_shade_dot_size = 0.02,
    graph_shade_dot_density = 20,
    legend_user_text="<font colour='green'> dot </font>"
        ,
    )

# define  the  data
hatch_input = minput(input_date_x_values =x,
	input_date_x2_values = x,
	input_y_values=y2,
	input_y2_values=y3)

hatch = mgraph(
    graph_type='bar',
    graph_bar_justification='centre',
    graph_bar_colour='pink',
    graph_bar_width=3 * 3600.,
    legend='on',
    graph_shade_style = "hatch",
    graph_shade_hatch_index = 4,
    legend_user_text="<font colour='pink'> hatch </font>"
        ,
    )
# Define the graph
title = mtext(text_lines=['Shading and Bar...'],
              text_justification='left', text_font_size=1.,
              text_colour='charcoal')

# To the plot
plot(
    output,
    projection,
    vertical,
    horizontal,
    solid_input, solid,
    dot_input, dot,
    hatch_input, hatch,
    title,
    )
#for the documentation
tofortran (
	ref, 
    output,
    projection,
    vertical,
    horizontal,
     solid_input, solid,
    dot_input, dot,
    hatch_input, hatch,
    title,
    )

tomv4 (
	ref, 
    solid,
    dot,
    hatch,
	)
tohtml (
	ref, 
    solid_input, solid,
    dot_input, dot,
	)
