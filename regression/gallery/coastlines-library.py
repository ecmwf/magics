# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


ref = 'coastlines-library'
#Setting of the output file name
output = output(output_formats = ['png'],output_name_first_page_number = "off",output_name = ref)




#Setting the coordinates of the geographical area
projection = mmap(
          super_page_theme = "dark",
          subpage_map_library_area = 'on',
          subpage_map_area_name='south_pole',
          )


#Coastlines setting
background = mcoast(
        map_coastline_general_style = "background",
        )
#Import the z500 data
data =  mgrib(grib_input_file_name  = "2t.grib",
              )

foreground = mcoast(
        map_coastline_general_style = "foreground",
        )
grid = mcoast(
        map_coastline_general_style = "grid",
        )

#Define the simple contouring for z500
contour = mcont( contour_automatic_setting='web' )


#Title settings
title = mtext(
      text_lines = ["<font size='1'>Area library</font>"],
      text_justification = "left",
      text_font_size = 0.8,
      text_colour = "charcoal")

#To the plot
plot(output, projection, data, contour, background) #data, contour, foreground)
#PLOt(output, data, contour)
#plot(output, projection, data, contour, foreground,  title)














