# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *
import json

#Example reference
ref = 'efas'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
area = mmap(subpage_upper_right_longitude= 20.,
                 subpage_upper_right_latitude= 70.,
                 subpage_lower_left_longitude= 0.,
                 subpage_lower_left_latitude= 60.)

#Background Coastlines
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'on',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'on',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast( map_grid= 'on',
                 map_grid_colour = 'tan',
                 map_label= 'off',
                 map_coastline_colour= 'tan',
                 map_coastline_land_shade= 'off',
                 map_coastline_sea_shade= 'off')


data =  mgeojson( geojson_input_type= 'file',
        geojson_input_filename = "efas.json",)
line = mline( legend = "off",
            polyline_thickness_list = [2., 5.],
            polyline_thickness_variable_name = "level",
            polyline_colour_level_list =  [1.,3., 4., 5., 6., 7.],
            polyline_thickness_level_list =  [1., 2,  7],
            polyline_colour_list =  ["green", "yellow", "orange", "red", "magenta"],
            polyline_colour_variable_name =  "level",
            polyline_effect_method =  "trajectory",
            polyline_transparency_level_list = [1., 7]
            )


title = mtext(text_lines = ["<font size='1'>GeoJSon First example</font>",
                        "",
                        "Cyclone"],
          text_justification = 'left',
          text_font_size = 0.8,
          text_colour = 'charcoal')

#add a legend
legend = mlegend(legend= 'on',
           legend_text_colour= 'black',
           legend_box_mode= 'positional',
           legend_box_x_position= 15.,
           legend_box_y_position= 0.5,
           legend_box_x_length= 4.,
           legend_box_y_length= 18.,
           legend_border= 'off',
           legend_border_colour= 'black',
           legend_box_blanking= 'on',
           legend_display_type= 'continuous',
           legend_title = "on",
       legend_title_text= "Wind",
       legend_text_font_size = "0.5")

#To the plot
plot(output, area, background, data, line, foreground, title, legend)

