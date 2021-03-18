# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


from Magics.macro import *

output = output(output_formats=['ps'],
                output_name_first_page_number='off',
                output_name="tephi_missing")

projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='tephigram',
    )


# Tephigram grid
tephi = mtephi( 
                thermo_isotherm_grid_colour = "magenta",
				thermo_isotherm_grid_thickness = 5,
				thermo_isotherm_label_font_style= "bold",
				thermo_isotherm_label_colour= "red",
				thermo_isotherm_label_frequency= 5,
				thermo_isotherm_label_font_size= 0.5)


tephi = mtephi()

# Curve

td = minput(
        input_x_values = [ -6., -16, -14, -27, -56,  1.7e+38, -71, -69,  1.7e+38],
        input_y_values = [675., 500, 0., 400,  280, 214, 200, 180, 155]
        )

graph = mgraph(
                 graph_line_colour            = "black",
                 graph_line_thickness         = 8,
                 graph_missing_data_mode      = "join",
                 graph_missing_data_colour    = "red",
                 graph_missing_data_thickness = 8
                )

                
plot(output,  projection, tephi, 
    td, graph, 
	)

