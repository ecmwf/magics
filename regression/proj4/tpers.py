# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


import numpy


ref = 'tpers'
#Setting of the output file name

png = output(output_formats = ['png'],
    output_name_first_page_number = "off",
    output_name = "%s"%(ref,))



#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'tpers',
subpage_map_projection_tilt = 0.,
subpage_map_projection_azimuth =  20.,
subpage_map_projection_view_latitude= 60.,
subpage_map_projection_view_longitude= -0.
	  )

#Coastlines setting
coast = mcoast( map_grid =  "off",
			map_grid_colour  =  "tan",
			map_coastline_land_shade  =  'on',
			map_coastline  =  'off',
			map_coastline_land_shade_colour  =  'cream',
			map_coastline_colour =  "tan",
			map_coastline_style = "solid")

#Title settings
title = mtext(
      text_lines = ["<font size='1'>tpers</font>"],
      text_justification = "left",
      text_font_size = 0.8,
      text_colour = "charcoal")

    
plot(png, projection, coast, title)










