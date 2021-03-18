# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *

#Example reference
ref = 'geowind'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
map = mmap( subpage_map_projection = 'geos',)

#Background Coastlines 
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'off',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'off',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast( 
                 map_grid_colour = 'tan',
                 map_coastline_colour= 'tan',
                 map_coastline_land_shade= 'off',
                 map_coastline_sea_shade= 'off')


#Import the  wind speed at 200hPa speed200 
data =  mgeo( geo_input_file_name = './testwind.geo',
               )



wind = mwind(
                legend= 'on',
                wind_field_type= 'arrows',
                wind_arrow_thickness= 2,
                wind_arrow_colour= 'evergreen' )


title = mtext(text_lines = ["<font size='1'>Geopoint inputs</font>",],
	      text_justification = 'left',
	      text_font_size = 0.5,
	      text_colour = 'charcoal')



#To the plot
#plot(output, map, foreground, wind, all, wind, thinning, title)

plot(output, map,  foreground, data, wind, title)
