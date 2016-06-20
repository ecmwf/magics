# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


from Magics.macro import *

output = output(output_formats=['png', 'ps'],
                output_name_first_page_number='off',
                output_name="streamlines")


# Setting the cartesian view
projection = mmap(
    
    )

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour= "white",
                     map_coastline_land_shade_colour= "cream",
                     map_grid= "on",
                     map_coastline_land_shade= "on",
                     map_coastline_sea_shade= "on",
                     map_label= "off",
                     map_coastline_colour= "tan")

#Import the  wind  at 200hPa uv200 
data =  mgrib(grib_input_file_name="wind.grb",
                grib_id= "wind")

wind = mwind(   wind_field_type= "streamlines",
                wind_streamline_thickness= 2,
                wind_streamline_min_density= 10.,
                wind_streamline_colour= "red"
                )




plot(output, projection, background, 
	data, wind
	)
