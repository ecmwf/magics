C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics

C Define the colours for the shading
      character*25 colours
      dimension colours(64)

C Define the title
      character*500 title
      dimension title(2)

      data colours /  'HSL(0,0,1)', 
     +    'HSL(29,0.14,0.92)', 
     +    'HSL(29,0.29,0.83)', 
     +    'HSL(29,0.43,0.75)', 
     +    'HSL(300,0.08,0.92)', 
     +    'HSL(360,0.16,0.84)', 
     +    'HSL(13,0.3,0.75)', 
     +    'HSL(18,0.44,0.67)', 
     +    'HSL(300,0.16,0.83)', 
     +    'HSL(340,0.22,0.75)', 
     +    'HSL(360,0.34,0.67)', 
     +    'HSL(8,0.47,0.58)', 
     +    'HSL(300,0.24,0.75)', 
     +    'HSL(330,0.28,0.67)', 
     +    'HSL(349,0.38,0.58)', 
     +    'HSL(360,0.5,0.5)', 
     +    'HSL(180,0.17,0.92)', 
     +    'HSL(120,0.08,0.84)', 
     +    'HSL(57,0.17,0.75)', 
     +    'HSL(44,0.3,0.67)', 
     +    'HSL(209,0.14,0.84)', 
     +    'HSL(187,0,0.75)', 
     +    'HSL(29,0.15,0.67)', 
     +    'HSL(29,0.29,0.59)', 
     +    'HSL(239,0.16,0.75)', 
     +    'HSL(299,0.08,0.67)', 
     +    'HSL(360,0.17,0.58)', 
     +    'HSL(13,0.3,0.5)', 
     +    'HSL(258,0.21,0.67)', 
     +    'HSL(299,0.16,0.59)', 
     +    'HSL(341,0.22,0.5)', 
     +    'HSL(360,0.33,0.42)', 
     +    'HSL(180,0.34,0.83)', 
     +    'HSL(161,0.22,0.75)', 
     +    'HSL(120,0.16,0.67)', 
     +    'HSL(78,0.21,0.58)', 
     +    'HSL(193,0.3,0.75)', 
     +    'HSL(180,0.17,0.67)', 
     +    'HSL(120,0.08,0.58)', 
     +    'HSL(59,0.16,0.5)', 
     +    'HSL(209,0.29,0.67)', 
     +    'HSL(209,0.15,0.58)', 
     +    'HSL(217,0,0.5)', 
     +    'HSL(29,0.14,0.42)', 
     +    'HSL(224,0.3,0.58)', 
     +    'HSL(237,0.17,0.5)', 
     +    'HSL(299,0.08,0.42)', 
     +    'HSL(360,0.16,0.33)', 
     +    'HSL(180,0.5, 0.75)', 
     +    'HSL(169,0.38,0.67)', 
     +    'HSL(150,0.28,0.58)', 
     +    'HSL(120,0.24,0.5)', 
     +    'HSL(188,0.47,0.67)', 
     +    'HSL(180,0.34,0.59)', 
     +    'HSL(160,0.22,0.5)', 
     +    'HSL(120,0.16,0.42)', 
     +    'HSL(198,0.44,0.58)', 
     +    'HSL(193,0.3,0.5)', 
     +    'HSL(180,0.17,0.42)', 
     +    'HSL(120,0.08,0.33)', 
     +    'HSL(209,0.43,0.5)', 
     +    'HSL(209,0.29,0.42)', 
     +    'HSL(209,0.14,0.33)', 
     +    'HSL(191,0,0.25)' /
C Open Magics
      call popen

      call psetc('output_name', 'cloud_cover_asia')
      call psetc('legend', 'off')

C Set the geographical area:
C Here we use Asia
      call psetr('subpage_lower_left_longitude', 55.000000)
      call psetr('subpage_upper_right_longitude', 175.000000)
      call psetr('subpage_upper_right_latitude', 80.000000)
      call psetc('subpage_map_projection', 'cylindrical')
      call psetr('subpage_lower_left_latitude', 0.000000)

C load the grib data
      call psetc('grib_input_file_name', '../cloud_cover.grb')
      call pgrib

c define the shading
      call psetc('contour_level_selection_type', 'interval')
      call psetc('contour_shade_colour_method', 'list')
      call psetc('contour_shade_technique', 'cell_shading')
      call psetc('contour_shade', 'on')
      call psetr('contour_reference_level', -0.500000)
      call psetc('contour_hilo', 'off')
      call psetr('contour_min_level', -0.500000)
      call psetr('contour_max_level', 63.500000)
      call psetc('contour', 'off')
      call psetr('contour_interval', 1.000000)
      call pset1c('contour_shade_colour_list', colours, 64)
      call pcont


C Add the coastlines
      call psetc('map_grid_colour', 'tan')
      call psetc('map_grid', 'on')
      call psetc('map_coastline_colour', 'tan')
      call pcoast

C Add a title
      call psetc('text_mode', 'positional')
      call psetr('text_box_y_length', 2.500000)
      call psetc('text_justification', 'left')
      call psetc('text_border', 'on')
      title(1)="Cloud Cover valid for <grib_info key='valid-date' />" 
      title(2)="<font colour='HSL(29,0.43,0.75)'> Low </font>"//
     +           '<font colour="HSL(360,0.5,0.5)"> L+M </font>'//
     +           '<font colour="HSL(300,0.24,0.75)"> Medium </font>'//
     +           '<font colour="HSL(209,0.43,0.5)"> M+H </font>'//
     +           '<font colour="HSL(180,0.5, 0.75)"> High </font>'//
     +           '<font colour="HSL(120,0.24,0.5)"> H+L </font>' 

      call pset1c('text_lines', title, 2)
      call psetr('text_box_x_length', 20.000000)
      call psetr('text_box_x_position', 1.500000)
      call psetc('text_html', 'true')
      call psetc('text_box_blanking', 'on')
      call psetc('text_border_colour', 'black')
      call psetr('text_box_y_position', 15.000000)
      call psetc('text_colour', 'black')
      call ptext



      call pclose

      end
