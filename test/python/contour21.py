# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'contour21'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
		output_name = ref)

#Setting the coordinates of the geographical area
area = mmap(subpage_upper_right_longitude= 5.,
                 subpage_upper_right_latitude= 45.,
                 subpage_lower_left_longitude= -10.,
                 subpage_map_projection= 'cylindrical',
                 subpage_lower_left_latitude= 35.)


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
    netcdf_x_component_variable = "uo",
    netcdf_y_component_variable = "vo",
  netcdf_filename = "missing.nc")


#Define the simple contouring for z500
wind = mwind( legend = "off",
                     wind_field_type               = 'arrows',
                     wind_thinning_factor          = 1.0,
                     
                     wind_arrow_unit_velocity      = 10.0,
                     wind_arrow_thickness          = 1
                )





title = mtext(
           text_lines = ["<font size='1'>Loading NetCDF...</font>",
		   			"<font colour='evergreen'>contour_level_selection_type = count</font> ", 
					"    calculate a reasonable  contour interval from the min and max of the displayed field"],
		   text_justification = "left",
		   text_font_size = 0.8,
           text_colour =  "charcoal")


#To the plot
plot(output, area, coast, data, wind, title)















