# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'cmf_graph'
#Setting of the output file name
output = output(output_formats = ['png'],
        output_name_first_page_number = "off",
        output_name = ref)

#Setting the cartesian view
projection = mmap( subpage_y_position= 2.,
            page_id_line = "off",
            subpage_map_projection = 'cartesian',
            subpage_x_axis_type = 'date',
            subpage_y_axis_type = 'regular',
            subpage_x_automatic = "on",
            subpage_y_automatic = "on")

#Vertical axis
vertical = maxis(axis_orientation = "vertical",
                 axis_type = "regular",
                 axis_tick_label_height = 0.4,
                 axis_tick_label_colour = 'navy',
                 axis_grid =  "on",
                 axis_grid_colour = "grey",
                 axis_grid_thickness = 1,
                 axis_grid_line_style = "dot")

#Horizontal axis
horizontal = maxis(axis_orientation = "horizontal",
                 axis_type = "date",
                 axis_date_type = "automatic",
                 axis_grid =  "on",
                 axis_days_label_height = 0.40,
                 axis_months_label_height = 0.40,
                 axis_years_label_height = 0.50,
                 axis_grid_colour = "grey",
                 axis_grid_thickness = 1,
                 axis_grid_line_style = "dot")



netcdf = mnetcdf(netcdf_type = "xypoint",
                netcdf_filename = "cmf.nc",
                netcdf_y_variable = "TP",
                netcdf_x_variable = "time"
                    )

#Define the graph
curve = mgraph(
            graph_line_colour='evergreen',
            graph_line_thickness=4,
            legend =  "on",
            legend_user_text =  "<font colour='blue' size='0.5'> MyCurve </font>"
            )




#To the plot
plot(output, projection, vertical, horizontal, netcdf, curve)











