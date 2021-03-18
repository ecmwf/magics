# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'octrahydro'

#Setting of the output file name
output = output(output_formats = ['png'],
        output_name_first_page_number = "off",
        output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap( 
                    page_id_line ='off',
                    subpage_lower_left_latitude = -3.,
                    subpage_lower_left_longitude = -50.,
                    subpage_upper_right_latitude = 3.,
                    subpage_upper_right_longitude = -40.,
                    subpage_map_projection =  "cylindrical")


#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
                map_coastline_land_shade  =  'on',
                map_coastline_land_shade_colour  =  'cream',
                map_coastline_colour =  "tan")


#Import the z500 data
data =  mgrib(grib_input_file_name  = "octrahydro.grib")



#Define the simple contouring for z500
contour = mcont( legend = 'on',
                contour = 'on',
                contour_label = 'on',
                contour_shade = 'off',
                contour_method = 'linear',
                contour_shade_method = "area_fill",
                contour_shade_technique = "grid_shading",
                contour_grid_value_plot = "on",
                contour_grid_value_plot_type = "value",
                contour_grid_value_colour = "rgb(0,0,0)",

                )





title = mtext(
           text_lines = ["<magics_title/>"],
           text_justification = "left",
           text_font_size = 0.6,
           text_colour =  "charcoal")


#To the plot
#plot(output, projection, coast, data, contour, mcoast(), title)
plot(output, projection, coast, data, contour, title)














