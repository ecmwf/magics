#!/usr/bin/env python
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


import Magics.macro as mag


width = 27.0
height = (2*width)/4
green ='rgb(0.5,1,0)'
yellow ='rgb(1,1,0.52)'
red ='rgb(1,0.3125,0.4492)'
purple ='rgb(0.8945312,0.5976562,0.8945312)'
LETW=70.0
LegSymbHeight = 0.45


myoutput = mag.output(output_formats = ['png'],
           output_name_first_page_number = 'off',
           output_name = 'efas_graph')




vertical = mag.maxis(
           axis_orientation='vertical',
           axis_type='regular',
           axis_tick_label_height=0.70,
           axis_tick_label_colour='navy',
           axis_grid='on',
           axis_grid_colour='grey',
           axis_grid_thickness=1,
           axis_grid_line_style='dot',
           axis_title='on',
           axis_title_text='Discharge (m3/s)',
           axis_title_height=0.7,
           #axis_title_font_style='bold',
           )

horizontal = mag.maxis(
           axis_orientation='horizontal',
           axis_type='date',
           axis_grid='on',
           axis_days_label_height=0.50,
           axis_months_label_height=0.50,
           axis_years_label_height=0.50,
           axis_grid_colour='grey',
           axis_grid_thickness=1,
           axis_grid_line_style='dot',
           axis_days_sunday_label_colour='black',
           axis_title='on',
           axis_title_text='Date',
           axis_title_height=0.7,
           #axis_title_font_style='bold',
           )

low_graph = mag.mgraph(graph_line_colour = green,
           graph_shade_colour = green,
           graph_line_thickness = 4,
           graph_type = 'area',
           graph_shade = 'on',
           legend = 'on',
           legend_text_font_size = LegSymbHeight,
           legend_user_text = "1.5-year")

medium_graph = mag.mgraph(graph_line_colour = yellow,
           graph_shade_colour = yellow,
           graph_line_thickness = 4,
           graph_type = 'area',
           graph_shade = 'on',
           legend = 'on',
           legend_text_font_size = LegSymbHeight,
           legend_user_text = '2-year')

high_graph = mag.mgraph(graph_line_colour = red,
           graph_shade_colour = red,
           graph_line_thickness = 4,
           graph_type = 'area',
           graph_shade = 'on',
           legend = 'on',
           legend_text_font_size = LegSymbHeight,
           legend_user_text = '5-year')

extreme_graph = mag.mgraph(graph_line_colour = purple,
           graph_shade_colour = purple,
           graph_line_thickness = 4,
           graph_type = 'area',
           graph_shade = 'on',
           legend = 'on',
           legend_text_font_size = LegSymbHeight,
           legend_user_text = '20-year')


min_y = 0.0
max_y = 3.5
min_date = '2013-09-23 00:00:00'
max_date = '2013-09-30 00:00:00'
min_max_dates = [min_date, max_date]

dis_thlow = 1.0
dis_thmed = 2.0
dis_thhigh = 3.0
dis_thextr = 4.0

lows = [dis_thlow, dis_thlow]
mediums = [dis_thmed, dis_thmed]
highs = [dis_thhigh, dis_thhigh]
extremes = [min(dis_thextr, max_y),min(dis_thextr, max_y)]

low_input = mag.minput(input_x_type = 'date',
           input_date_x_values = min_max_dates,
           input_date_x2_values = min_max_dates,
           input_y_values = lows,
           input_y2_values = mediums)

medium_input = mag.minput(input_x_type = 'date',
           input_date_x_values = min_max_dates,
           input_date_x2_values = min_max_dates,
           input_y_values = mediums,
           input_y2_values = highs)

high_input = mag.minput(input_x_type = 'date',
           input_date_x_values = min_max_dates,
           input_date_x2_values = min_max_dates,
           input_y_values = highs,
           input_y2_values = extremes)

extreme_input = mag.minput(input_x_type = 'date',
           input_date_x_values = min_max_dates,
           input_date_x2_values = min_max_dates,
           input_y_values = extremes,
           input_y2_values = [max_y, max_y])


projection = mag.mmap(
           super_page_x_length = width+3,
           super_page_y_length = height+4,
           subpage_x_position = 2.2,
           subpage_y_position = 2.8,
           subpage_x_length = width,
           subpage_y_length = height,
           subpage_map_projection = 'cartesian',
           subpage_x_axis_type = 'date',
           subpage_y_axis_type = 'regular',
           subpage_x_date_min = min_date,
           subpage_x_date_max = max_date,
           subpage_y_min = min_y,
           subpage_y_max = max_y,
           page_id_line = "off")


mag.plot(myoutput, projection, vertical, horizontal,
           low_input, low_graph,
           medium_input, medium_graph,
           high_input, high_graph,
           extreme_input, extreme_graph,
         )
