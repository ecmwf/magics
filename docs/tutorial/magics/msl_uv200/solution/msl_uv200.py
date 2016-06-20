# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *

#Setting of the output file name
output = output({"output_formats":['ps', 'png'], 
			'output_name':'msl_uv_australia'})

#Setting the coordinates of the geographical area
australia = mmap({
				    "subpage_upper_right_longitude": 190.,
                    "subpage_upper_right_latitude": -5.,
                    "subpage_lower_left_longitude": 80.,
                    "subpage_map_projection": "cylindrical",
                    "subpage_lower_left_latitude": -55.})


#Background Coastlines 
background = mcoast( {"map_coastline_sea_shade_colour": "white",
                     "map_coastline_land_shade_colour": "cream",
                     "map_grid": "off",
                     "map_coastline_land_shade": "on",
                     "map_coastline_sea_shade": "on",
                     "map_label": "off",
                     "map_coastline_colour": "tan"})

#Foreground Coastlines
foreground = mcoast( { "map_grid": "on",
                 "map_grid_colour" : "tan",
                 "map_label": "off",
                 "map_coastline_colour": "tan"})

#Import the z500 data 
msl =  mgrib({ "grib_input_file_name" : "../msl.grb",
                "grib_id": "z500"})

#Define the simple contouring for msl
msl_contour = mcont({
				"legend":"off", 
				"contour_level_selection_type": "interval",
                "contour_interval": 5.,
                "contour_line_colour": "black",
                "contour_line_thickness": 1,
                "contour_hilo": "on",
                "contour_hilo_quality": "high",
                "contour_hi_colour": "black",
                "contour_lo_colour": "black",
                "contour_hilo_height": 0.25,
                "contour_label": "off",
                "contour_highlight_colour": "black",
                "contour_highlight_thickness": 2,
                })


#Import the  wind speed at 200hPa speed200 
speed200 =  mgrib({ "grib_input_file_name" : "../speed200.grb",
                "grib_id": "t850"})


#Define the shading for the wind speed
speed200_contour = mcont({
    "legend": "on",
    "contour_level_selection_type": "level_list", 
    "contour_level_list": [30., 40., 50., 60., 70., 80., 90., 100.], 
    "contour_shade": "on", 
    "contour_shade_max_level_colour": "evergreen", 
    "contour_shade_min_level_colour": "yellow",
    "contour_shade_method": "area_fill", 
    "contour_reference_level": 0., 
    "contour_highlight": "off", 
    "contour_hilo": "on", 
    "contour_hilo_format": "(F3.0)", 
    "contour_hilo_height": 0.2, 
    "contour_hilo_type": "number", 
    "contour_hilo_suppress_radius": 30., 
    "contour_hi_min_value": 15., 
    "contour_label": "off" 
         })


#Import the  wind  at 200hPa uv200 
uv200 =  mgrib({ "grib_input_file_name" : "../uv200.grb",
                "grib_id": "uv200"})

uv200_wind = mwind({
                "legend": "on",
                "wind_field_type": "arrows",
                "wind_arrow_thickness": 1,
                "wind_thinning_factor": 3.,
                "wind_arrow_colour": "gold" })

lines =["My first line", "My second line"]

title = ptext({
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
           "text_mode": "positional",
           "text_box_x_position": 1.5,
           "text_box_y_position": 12.,
           "text_box_x_length": 20.,
           "text_box_y_length": 2.5,
           "text_border": "on",
           "text_border_colour": "black",
           "text_box_blanking": "on",
           "text_justification" : "left"})

#add a legend
legend = mlegend({"legend":"on",
           "legend_text_colour":"black",
           "legend_box_mode": "positional",
           "legend_box_x_position": 27.,
           "legend_box_y_position": 0.75,
           "legend_box_x_length": 2.,
           "legend_box_y_length": 12.,
           "legend_border": "on",
           "legend_border_colour": "black",
           "legend_box_blanking": "on",
           "legend_display_type": "continuous" })

#To the plot
plot(output, australia, background, 
		speed200, speed200_contour,
		uv200, uv200_wind,
		msl, msl_contour, 
		foreground, title, legend)
