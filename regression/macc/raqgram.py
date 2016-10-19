# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module

from Magics.macro import *

# Setting of the output file name



param1 = "O3_USI"
param2 = "NO2_USI"
param3 = "SO2_USI"
param4 = "PM10_USI"

height = 5.5
y = 21.
output = output(output_formats=['png'],  
                output_name_first_page_number = "off",
                output_name='raqgram',
                super_page_y_length=29.7, super_page_x_length=21.)

page1 = page( layout='positional',
            page_x_length = 20., page_y_length = 5.,
            page_y_position = 21.,
            page_id_line = 'off',
            subpage_y_length=4., subpage_y_position=0.)

# define the axis
haxis = maxis(  axis_grid= "on",
                axis_grid_colour= "Rgb(0.5, 0.5, 0.5)",
                axis_grid_line_style= "dash",
                axis_line_colour= "black",
                axis_minor_tick= "on",
                axis_minor_tick_colour= "black",
                axis_tick_colour= "black",
                axis_type= "date",
            )

title = mtext(
    text_colour='black',
    text_font_size=0.4,
    text_mode='positional',
    text_box_x_position=1.,
    text_box_y_position=4.1,
    text_justification='left',
    )

vaxis = maxis( axis_grid = "on",
            axis_grid_colour= "grey",
            axis_grid_line_style= "dash",
            axis_grid_reference_level= 0.,
            axis_grid_reference_thickness= 1,
            axis_line= "on",
            axis_line_colour= "black",
            axis_tick_colour= "black",
            axis_tick_label_colour= "black",
            axis_orientation = 'vertical')


projection = mmap(
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_x_automatic='on',
    subpage_y_axis_type='regular',
    subpage_y_automatic='on',
    )





data1 = mepsinput(epsxml_input_filename = "raqgramdata_%s.xml" % (param1,),
                epsxml_parameter= param1,
                epsxml_long_title='on')
box1 =   mepsgraph(eps_box_border_colour = 'black',
               eps_box_border_thickness = 2,
               eps_box_quantiles_colour= ['peach', 'green'],
               eps_font_colour='black',
               eps_box_width=1.)

y = y - height;

page2 = page( layout='positional',
            page_x_length = 20., page_y_length = 5.,
            page_y_position = y,
            page_id_line = 'off',
            subpage_y_length=4., subpage_y_position=0.)

data2 = mepsinput(epsxml_input_filename = "raqgramdata_%s.xml" % (param2,),
                epsxml_parameter= param2,
                epsxml_long_title='off')

box2 =   mepsgraph(eps_box_border_colour = 'black',
               eps_box_border_thickness = 2,
               eps_box_quantiles_colour= ['sky', 'navy', 'charcoal'],
               eps_font_colour='black',
               eps_box_width=1.)

y = y - height;

page3= page( layout='positional',
            page_x_length = 20., page_y_length = 5.,
            page_y_position = y,
            page_id_line = 'off',
            subpage_y_length=4., subpage_y_position=0.)

data3 = mepsinput(epsxml_input_filename = "raqgramdata_%s.xml" % (param3,),
                epsxml_parameter= param3,
                epsxml_long_title='off')

box3 =   mepsgraph(eps_box_border_colour = 'black',
               eps_box_border_thickness = 2,
               eps_box_colour= 'rgb(1.0, 0.85098039215686272, 0.50196078431372548)',
               eps_font_colour='black',
               eps_box_width=1.)
y = y - height;
page4 = page( layout='positional',
            page_x_length = 20., page_y_length = 5.,
            page_y_position = y,
            page_id_line = 'off',
            subpage_y_length=4., subpage_y_position=0.)

data4 = mepsinput(epsxml_input_filename = "raqgramdata_%s.xml" % (param4,),
                epsxml_parameter= param4,
                epsxml_long_title='off')

box4 =   mepsgraph(eps_box_border_colour = 'black',
               eps_box_border_thickness = 2,
               eps_box_colour= 'sky',
               eps_font_colour='black',
               eps_box_width=1.)



plot(
    output,
    page1, projection, haxis, vaxis, data1, box1, title,
    page2, projection, haxis, vaxis, data2, box2, title,
    page3, projection, haxis, vaxis, data3, box3, title,
    page4, projection, haxis, vaxis, data4, box4, title,
    )
