# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# importing Magics module
from Magics.macro import *

ref = 'obsjson'

# Setting of the output file name

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area

projection = mmap(
    subpage_x_length=24.,
    subpage_upper_right_longitude=50.00,
    subpage_upper_right_latitude=65.00,
    subpage_lower_left_latitude=25.00,
    subpage_lower_left_longitude=-20.0,
    subpage_map_projection='cylindrical',
    )

# Coastlines setting

coast = mcoast(map_grid='on', map_grid_colour='grey',
               map_grid_thickness=2,
               map_coastline_colour='RGB(0.4,0.4,0.4)',
               map_coastline_thickness=3)


obs = mobs(
	obsjson_info_list = ['{"type": "ersagun", "identifier": "era1", "temperature": -3.0, \
     "pressure_after": 1008.0, "pressure_before": 1008.0,\
     "pressure": 1010.0, "longitude": 0.3, \
     "latitude": 49.5, "temperature_before": -2.0}',
     '{"type": "ersagun","identifier": "era2", "temperature": -5.0, \
     "pressure_after": 1038.0, "pressure_before": 999.0,\
     "pressure": 1010.0, "longitude": 5.39, \
     "latitude": 55., "temperature_before": -2.0}'
     ],
	obs_template_file_name = "obs.template",
    obs_size=0.3,
    obs_ring_size=0.2,
	obs_distance_apart = 0.
	)

title = mtext(text_lines=["Observation plotting ..."  ], 
		text_justification='left', text_font_size=0.8,
           
         text_colour='charcoal')


# To the plot
plot( 
    output,
    projection,
	obs,
    coast,
    title,
    )
