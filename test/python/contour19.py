# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'contour19'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'cylindrical')


#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
				map_coastline_land_shade  =  'on',
				map_coastline_land_shade_colour  =  'cream',
				map_coastline_colour =  "tan")


#Import the data 
data =  mnetcdf(netcdf_type = "geomatrix",
    netcdf_x_variable = "latitude",
    netcdf_y_variable = "longitude",
    netcdf_value_variable = "u10",
  netcdf_filename = "era5_2000_aug_1.nc")


#Define the simple contouring for z500
contour = mcont( legend = "off",
                contour_line_colour = "navy",
                contour_line_thickness =  2,
                contour_label =  "on",
                contour_highlight_colour =  "navy",
                contour_highlight_thickness =  6 )





title = mtext(
           text_lines = ["<font size='1'>Loading NetCDF...</font>",
		   			"<font colour='evergreen'>contour_level_selection_type = count</font> ", 
					"    calculate a reasonable  contour interval from the min and max of the displayed field"],
		   text_justification = "left",
		   text_font_size = 0.8,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, coast, data, contour, title)















