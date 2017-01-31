# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module
from Magics.macro import *

ref = 'gradients'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)


#Setting the coordinates of the geographical area
projection = mmap(
                subpage_map_projection = 'robinson')

coast = mcoast(map_grid='on', map_grid_colour='tan',
               map_coastline_land_shade='on',
               map_coastline_land_shade_colour='cream',
               map_coastline_colour='tan')

# Import the data
data = mgrib(grib_input_file_name='t850.grb')

# Define a contour
list = mcont(contour_highlight='off',
            contour='off',
            contour_label='off',
            contour_shade                  = 'on',
            contour_level_count            = 5,
            contour_shade_colour_method    = 'gradients',
            #contour_shade_colour_method    = 'calculate',
            contour_gradients_colour_list = ["RGB(0,0,1)", "white",  "red"],
            contour_gradients_value_list = [-50., 0, 50.],
            contour_gradients_step_list = [30,  30],
            contour_gradients_technique_list = ["linear", "linear"],
            contour_shade_method           = 'area_fill',
            legend                         = 'on',)

legend = mlegend(legend = "on",
         legend_text_colour="black",
         legend_box_mode= "automatic",
         #legend_automatic_position= "right",
         legend_display_type = "continuous",
         legend_title = "on",
         legend_entry_border = 'off',
         legend_title_text= "Temperature at 850 hPa",
         legend_values_list= [-50., -25, 0, 25, 50],
         legend_text_composition = 'user_text_only', 
         legend_user_maximum = "on",
         legend_user_maximum_text = "50",
         legend_text_font_size = 0.5)

title = mtext(
           text_lines = ["<font size='1'>Gradients technique for shading </font>",],
           text_justification = "left",
           text_font_size = 0.8,
           text_colour =  "charcoal")

# To the plot
plot(
    output,
    #projection,
    coast, data, list,
    title, legend,
    )


