# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from Magics.macro import *
#setting the output
output = output(   
                output_formats = ['png2'],
                output_name = "netcdf_contwind",
                output_name_first_page_number = "off"
        )
# Setting the cartesian view
projection = mmap(
       
        )
        
coastlines = mcoast()

tempe = mnetcdf(netcdf_filename = "fc_surf.nc",
      netcdf_value_variable = "v2t",
      netcdf_type = "geomatrix",

    )
contour = mcont()
wind_data = mnetcdf(netcdf_filename = "fc_surf.nc",
		netcdf_type = "geomatrix",
		netcdf_x_component_variable = "v10u",
		netcdf_y_component_variable = "v10v",
		netcdf_colour_component_variable = "v2t"
		)

wind = mwind(wind_advanced_method = "on",
        wind_advanced_colour_parameter = "parameter",
        wind_field_type='flags'
  ) 

title = mtext(
        text_lines= ['Example of NetcdfData contour and Wind...',
        '<magics_title/>'],
        text_html= 'true',
        text_justification= 'left',
        text_font_size= 0.8,
        text_colour= 'charcoal',
        )
 
plot(output, projection, coastlines, tempe, contour, wind_data, wind, title)
