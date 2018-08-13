# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *

ref = 'netcdf_contour_python'

#Setting of the output file name
output = output(output_formats = ['png'],
		output_name_first_page_number = "off",
		output_name = ref)

#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
				map_coastline_land_shade  =  'on',
				map_coastline_land_shade_colour  =  'cream',
				map_coastline_colour =  "tan")

#Import data
t2m = mnetcdf(
  netcdf_type = "geomatrix",
  netcdf_filename = "era5_2mt.nc",
  netcdf_value_variable = "t2m "
)

#Define the simple contouring
contour = mcont(legend = "off",
                contour_line_colour = "navy",
                contour_line_thickness =  2,
                contour_label =  "on",
                contour_highlight_colour =  "navy",
                contour_highlight_thickness =  6 )

#To the plot
plot(output, coast, t2m, contour)
