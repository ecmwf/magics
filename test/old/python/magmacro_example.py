# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from Magics.macro import *

#Definition of the output formats
formats = output({'output_formats':['ps','svg'],
                  'output_name':'magmacro_example'})

#Setting the coordinates of the geographical area
europe = pmap({
                "subpage_upper_right_longitude": 65.,
                "subpage_map_projection": "polar_stereographic",
                "subpage_map_vertical_longitude": 0.,
                "subpage_lower_left_longitude": -37.27,
                "subpage_lower_left_latitude": 18.51,
                "subpage_upper_right_latitude": 51.28})

coastlines = mcoast({'map_coastline_land_shade' : 'on',
                     'map_coastline_land_shade_colour' : 'grey',
                     'map_coastline_sea_shade' : 'on',
                     'map_coastline_sea_shade_colour' : 'white'})

#Import the z500 data
z500 =  pgrib({ "grib_input_file_name" : "../data/z500.grb"})

#Define the simple contouring for z500
z500_contour = mcont({
                "contour_level_selection_type": "interval",
                "contour_line_colour": "black",
                "contour_hilo_height": 0.25,
                "contour_line_thickness": 1,
                "contour_highlight_colour": "red",
                "contour_highlight_thickness": 2,
                "contour_interval": 5.})

#Do the plot
plot(formats, europe, coastlines, z500, z500_contour)
