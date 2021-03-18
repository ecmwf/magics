# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'taylor'
#Setting of the output file name
output = output(output_formats = ['png'],
        output_name_first_page_number = "off",
        output_name = ref)

#Setting the cartesian view
projection = mmap(subpage_map_projection = 'taylor',
            subpage_frame='off',
            taylor_standard_deviation_min = 0.,
            taylor_standard_deviation_max= 2.
            )

#Vertical axis
vertical = maxis(
                axis_line = "on",
                axis_line_colour = "grey",
                axis_title = "on",
                axis_title_text = "stdev ratio",
                axis_tick_interval = 2.,
                axis_orientation = "vertical",
                )

#Horizontal axis
horizontal = maxis(
                axis_line = "on",
                axis_line_colour = "grey",
                axis_title = "on",
                axis_title_text = "stdev ratio",
                axis_tick_interval =  2.)


grid  = mtaylor ( taylor_label_colour = "black",
                 taylor_label_height = 0.5,
                 taylor_reference_line_colour = "charcoal",
                 taylor_reference_line_thickness = 3,
                 taylor_reference_line_style = "solid",
                 taylor_primary_grid_reference = 1.,
                 taylor_primary_grid_increment = 0.2,
                 taylor_primary_grid_line_style = "dash",
                 taylor_primary_grid_line_colour = "black",
                 taylor_secondary_grid = "on",
                 taylor_secondary_grid_line_colour = "evergreen",
                 taylor_secondary_grid_line_style = "dot",
                 taylor_secondary_grid_reference = 1.,
                 taylor_secondary_grid_increment = 0.2
            )

data = mwrepjson( wrepjson_family =  "data",
                wrepjson_input_filename = "taylordata.json",
                wrepjson_keyword =  "data",
                wrepjson_missing_value =  -999.
                )

symbol = msymb ( symbol_type = "marker",
                 symbol_table_mode  = "off",
                 symbol_marker_index = 15,
                 symbol_colour  = "red",
                 symbol_height = 0.5,
                 legend =  "on",
                 legend_user_text =  "obs"
                )

lines =["Taylor example..."]

title = mtext(
           text_lines = lines,
           text_justification  =  "left",
           text_font_size =  1.,
           text_colour = "charcoal")

legend = mlegend()

#To the plot
plot(output, projection, vertical, horizontal, grid, data, symbol, title)
#plot(output, projection, vertical, horizontal, grid, title)














