#!/usr/local/bin/python

import Magics.macro as magics
import gribapi as grib 
import numpy as numpy 
import json 


#Setting of the output file name
output = magics.output(output_formats= ['png'], output_name='mars-array')

#Setting the projection attributes
europe =  magics.mmap(
       		subpage_lower_left_longitude = -20.,
	   		subpage_upper_right_longitude = 20.00,
			subpage_upper_right_latitude = 30.,
			subpage_map_projection = "cylindrical",
			subpage_lower_left_latitude = 70.)
			


file =  open('z500.grb')

#Getting the first  message from the file
field = grib.grib_new_from_file(file)

nj = grib.grib_get(field,"Nj")
ni = grib.grib_get(field,"Ni")


metadata = { "paramId" : grib.grib_get(field,"paramId"),
             "units" : grib.grib_get(field,"units"),
             "typeOfLevel": grib.grib_get(field,"typeOfLevel"),  
             "marsType": grib.grib_get(field,"marsType"),  
             "marsClass": grib.grib_get(field,"marsClass"),  
             "marsStream": grib.grib_get_string(field,"marsStream"),
             "level": grib.grib_get(field,"level") } 

firstlat = grib.grib_get(field, "latitudeOfFirstGridPointInDegrees")
steplat = -grib.grib_get(field, "jDirectionIncrementInDegrees")

firstlon = grib.grib_get(field, "longitudeOfFirstGridPointInDegrees")
steplon = grib.grib_get(field, "iDirectionIncrementInDegrees")

  
#Getting the field values
values = grib.grib_get_values(field).reshape(grib.grib_get(field,"Nj"), grib.grib_get(field,"Ni"))


#Convert from Kelvin to Celsius
values = values 

    
data = magics.minput( input_field =   values,
                  input_field_initial_latitude = firstlat,
                  input_field_latitude_step = steplat,
                  input_field_initial_longitude = firstlon,
                  input_field_longitude_step = steplon,
                  input_mars_metadata = json.dumps(metadata),
                     )

#Setting the field contour 
contour = magics.mcont( 
                contour_shade=                   "on",
                legend=                          "on",
                legend_display_type="continuous",
                contour_highlight = "off",
                contour_shade_method =           "area_fill",
                contour_shade_colour_direction = "clockwise",
                contour_shade_colour_method  =   "calculate",
                contour_shade_max_level_colour=  "red",
                contour_shade_min_level_colour= " blue")
                
eccharts = magics.mcont(contour_automatic_setting = "ecchart")
#Setting the title
title = magics.mtext(text_lines=["Using Grib API and arrays..."], 
			text_colour="charcoal",
			text_font_size='0.8',
			text_justification='left')



#Plot the map
magics.plot(output, europe, data, eccharts, magics.mcoast(), title)



