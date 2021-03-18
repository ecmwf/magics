# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *
import json

#Example reference
ref = 'geojson_input'

#Setting of the output file name
output = output(output_formats= ['png'],
                output_name_first_page_number= 'off',
                output_name= ref)

#Setting the coordinates of the geographical area
area = mmap(subpage_upper_right_longitude= 180.,
                 subpage_upper_right_latitude= 90.,
                 subpage_lower_left_longitude= -180.,
                 subpage_lower_left_latitude= -90.)

#Background Coastlines
background = mcoast( map_coastline_sea_shade_colour= 'white',
                     map_coastline_land_shade_colour= 'cream',
                     map_grid= 'off',
                     map_coastline_land_shade= 'on',
                     map_coastline_sea_shade= 'on',
                     map_label= 'off',
                     map_coastline_colour= 'tan')

#Foreground Coastlines
foreground = mcoast( map_grid= 'on',
                 map_grid_colour = 'tan',
                 map_label= 'off',
                 map_coastline_colour= 'tan',
                 map_coastline_land_shade= 'off',
                 map_coastline_sea_shade= 'off')

#Import the z500 data
data = {
  "coordinates": [
    [
      [
        -112.4751265395822,
        49.22009389532161
      ],
      [
        -116.62509597124857,
        53.309626538152095
      ],
      [
        -118.9151697373996,
        49.43534662203797
      ]
    ],
    [
      [
        -125.27468276491868,
        -70.54530630794772
      ],
      [
        -130.27468276491868,
        -80.54530630794772
      ],
      [
        -128.69964317766592,
        -79.42844281752712
      ]
    ],
    [
      [
        106.53537282393648,
        45.173527191962684
      ],
      [
        110.31188673144166,
        48.952815714617586
      ],
      [
        105.10685624590496,
        56.823948318072986
      ],
      [
        100.89710323992226,
        60.471269981064836
      ],
      [
        120.61237115246117,
        48.225215772936494
      ]
    ]
   ],
  "properties": {
    "type": "warm fronts"
  },
  "type": "MultiLineString"
}

cold =  mgeojson( geojson_input_type= 'string',
        geojson_input = json.dumps(data),)
blue = mline(polyline_line_colour = "blue", polyline_line_thickness = 2)

title = mtext(text_lines = ["<font size='1'>GeoJSon First example</font>",
                        "",
                        "Warm fronts"],
          text_justification = 'left',
          text_font_size = 0.8,
          text_colour = 'charcoal')

#add a legend
legend = mlegend(legend= 'on',
           legend_text_colour= 'black',
           legend_box_mode= 'positional',
           legend_box_x_position= 15.,
           legend_box_y_position= 0.5,
           legend_box_x_length= 4.,
           legend_box_y_length= 18.,
           legend_border= 'off',
           legend_border_colour= 'black',
           legend_box_blanking= 'on',
           legend_display_type= 'continuous',
           legend_title = "on",
       legend_title_text= "Wind",
       legend_text_font_size = "0.5")

#To the plot
plot(output, background, cold, blue, foreground, title, legend)

