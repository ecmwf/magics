# importing Magics module

from Magics.macro import *

# Setting of the output file name

output = output(output_formats=['ps'], output_name='graph',
                super_page_y_length=29.7, super_page_x_length=21.)

# define the cartesian projection

# define the cartesian projection

roseprojection = mmap(
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_x_automatic='on',
    subpage_y_axis_type='regular',
    subpage_y_automatic='off',
    subpage_y_min=-43000.,
    subpage_y_max=43000.,
    subpage_y_position=2.,
    )

# define horizontal axis

horizontal = maxis(
    axis_orientation='horizontal',
    axis_type='date',
    axis_date_type='days',
    axis_days_label='both',
    axis_days_label_colour='Rgb(0.5, 0.5, 0.5)',
    axis_days_label_height=0.35,
    axis_grid='on',
    axis_grid_colour='Rgb(0.5, 0.5, 0.5)',
    axis_grid_line_style='solid',
    axis_line_colour='grey',
    axis_minor_tick='on',
    axis_minor_tick_colour='grey',
    axis_months_label_colour='Rgb(0.5, 0.5, 0.5)',
    axis_months_label_height=0.3,
    axis_tick_colour='grey',
    axis_years_label_colour='Rgb(0.5, 0.5, 0.5)',
    axis_years_label_height=0.3,
    )

# define vertical axis

vertical = maxis(
    axis_orientation='vertical',
    axis_grid='on',
    axis_grid_colour='Rgb(0.5, 0.5, 0.5)',
    axis_grid_line_style='dash',
    axis_line='on',
    axis_line_colour='grey',
    axis_tick='off',
    axis_tick_label='off',
    )

data = mwrepjson(wrepjson_family='eps',
                 wrepjson_input_filename='cloud-data.json',
                 wrepjson_parameter='164.128',
                 wrepjson_parameter_information='Total cloud cover')

rose = mepscloud(eps_rose_cloud_colour='black',
                 eps_rose_wind_border_colour='Rgb(0.5000, 0.5000, 0.5000)'
                 )

lines = ["<json_info key='parameter_info'/>",
         "Forecast from <json_info key='date'/> for location <json_info key='location'/>"
         ]

title = mtext(
    text_lines=lines,
    text_html='true',
    text_colour='black',
    text_font_size=0.4,
    text_mode='positional',
    text_box_x_position=1.,
    text_box_y_position=4.,
    text_box_x_length=20.,
    text_box_y_length=2.5,
    text_border='off',
    text_justification='left',
    )

newpage = page(page_x_length=20., page_y_length=5., page_id_line='off',
               subpage_y_length=2., subpage_y_position=2.)

# define the cartesian projection

epsprojection = mmap(
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_x_automatic='on',
    subpage_y_axis_type='regular',
    subpage_y_automatic='on',
    subpage_y_position=2.,
    )

automatic = maxis(
    axis_orientation='vertical',
    axis_grid='on',
    axis_grid_colour='Rgb(0.5, 0.5, 0.5)',
    axis_grid_line_style='dash',
    axis_line='on',
    axis_line_colour='grey',
    axis_tick='on',
    axis_tick_label='on',
    )
cc_clim = mwrepjson(
    wrepjson_family='eps',
    wrepjson_input_filename='cloud-cover-clim-data.json',
    wrepjson_keyword='clim',
    wrepjson_parameter='164.128',
    wrepjson_parameter_scaling_factor=8.,
    wrepjson_parameter_information='Total cloud cover',
    )

cc_shade = mepsshading(eps_shade_colour='Rgb(0.925, 0.609, 0.953)',
                       eps_shade_line_thickness=4)

cc = mwrepjson(wrepjson_family='eps',
               wrepjson_input_filename='cloud-cover-clim-data.json',
               wrepjson_keyword='eps', wrepjson_parameter='164.128',
               wrepjson_parameter_scaling_factor=8.)

cc_graph = mepsgraph(eps_box_border_colour='Rgb(0.5, 0.5, 0.5)',
               eps_box_border_thickness=2,
               eps_box_colour='Rgb(0.925, 0.609, 0.953)',
               eps_box_width=3.)

wave = mwrepjson(wrepjson_family='eps',
                 wrepjson_input_filename='wave-data.json',
                 wrepjson_parameter='230.140',
                 wrepjson_parameter_information='Wave')

wave_graph = \
    mepswave(eps_rose_wind_border_colour='Rgb(0.5000, 0.5000, 0.5000)',
             eps_rose_wind_colour='RGB(0.925,0.609,0.953)',
             eps_rose_wind_convention='oceanographic')

# To the plot

plot(
    output,
    newpage, epsprojection, horizontal, automatic, cc_clim, cc_shade, cc, cc_graph, title, 
	newpage, roseprojection, horizontal, vertical, wave, wave_graph, title,
    )

tohtml(cc_clim, cc_shade)
