C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics

C Defines the colours table for the shading
      character*25 colour
      dimension colour(24)

C Defines the lines for the title
      character*100 title
      dimension title(4)

      data colour /
     +    'rgb(0,0,0.5)', 
     +    'rgb(0,0,0.5)', 
     +    'rgb(0,0,0.85)', 
     +    'rgb(0,0,0.5)', 
     +    'rgb(0,0,0.85)', 
     +    'rgb(0.25,0,1)', 
     +    'blue_purple', 
     +    'greenish_blue', 
     +    'blue_green', 
     +    'bluish_green', 
     +    'yellow_green', 
     +    'greenish_yellow', 
     +    'yellow', 
     +    'orangish_yellow', 
     +    'orange_yellow', 
     +    'yellowish_orange', 
     +    'orange', 
     +    'reddish_orange', 
     +    'red_orange', 
     +    'orangish_red', 
     +    'red', 
     +    'magenta', 
     +    'magenta', 
     +    'magenta' /

      data title /
     +    "Forecast from <grib_info id='z500' key='base-date'/> ",
     +    "Valid for<grib_info id='z500' key='valid-date'/> ",
     +    "Parameter <grib_info id='z500' key='name'/> at 
     +<grib_info id='z500' key='level'/> hPa",
     +    "Parameter <grib_info id='t850' key='name'/> at 
     +<grib_info id='t850' key='level'/> hPa"/

C open magics
      call popen

C Setting of the output file name
      call psetc('output_name', 'z500_t850_europe')


C Setting the coordinates of the geographical area
      call psetr('subpage_upper_right_longitude', 62.000000)
      call psetr('subpage_upper_right_latitude', 58.0000)
      call psetr('subpage_lower_left_latitude', 21.510000)
      call psetr('subpage_map_vertical_longitude', 0.000000)
      call psetr('subpage_lower_left_longitude', -37.270000)
      call psetc('subpage_map_projection', 'polar_stereographic')

C Set the legend
      call psetr('legend_box_x_position', 27.5)
      call psetc('legend_display_type', 'continuous')
      call psetr('legend_box_y_position', 1.000000)
      call psetr('legend_box_x_length', 2.000000)
      call psetc('legend_box_mode', 'positional')
      call psetc('legend_text_colour', 'black')
      call psetr('legend_box_y_length', 13.000000)
      call psetc('legend', 'on')


C Import the t850 data
      call psetc('grib_id', 't850')
      call psetc('grib_input_file_name', '../t850.grb')
      call pgrib

C Define the shading for t850 
      call psetc('contour_level_selection_type', 'interval')
      call psetc('contour_shade_colour_method', 'list')
      call psetc('contour_shade_method', 'area_fill')
      call psetc('contour_shade', 'on')
      call psetr('contour_shade_max_level', 48.000000)
      call psetc('contour_hilo', 'off')
      call psetc('contour', 'off')
      call psetc('contour_label', 'off')
      call psetr('contour_shade_min_level', -48.000000)
      call psetr('contour_interval', 4.000000)
      call pset1c('contour_shade_colour_list', colour, 24)
      call pcont


C Import the z500 data
      call psetc('grib_id', 'z500')
      call psetc('grib_input_file_name', '../z500.grb')
      call pgrib

C Define the contouring for z500 
      call psetc('contour', 'on')
      call psetc('legend', 'off')
      call psetc('contour_shade', 'off')
      call psetc('contour_label', 'on')
      call psetc('contour_level_selection_type', 'interval')
      call psetc('contour_line_colour', 'black')
      call psetr('contour_hilo_height', 0.250000)
      call psetc('contour_hilo', 'on')
      call psetc('contour_hilo_quality', 'high')
      call pseti('contour_line_thickness', 1)
      call psetc('contour_label', 'off')
      call psetc('contour_highlight_colour', 'black')
      call pseti('contour_highlight_thickness', 2)
      call psetr('contour_interval', 5.)
      call pcont

C Add the coastlines 
      call psetc('map_grid_colour', 'tan')
      call psetc('map_grid', 'on')
      call psetc('map_coastline_colour', 'tan')
      call pcoast


C add a title
      call psetc('text_mode', 'positional')
      call psetr('text_box_y_length', 2.500000)
      call psetc('text_justification', 'left')
      call psetc('text_border', 'on')
      call pset1c('text_lines', title, 4)
      call psetr('text_box_x_length', 20.000000)
      call psetr('text_box_x_position', 1.500000)
      call psetc('text_html', 'true')
      call psetc('text_box_blanking', 'on')
      call psetc('text_border_colour', 'black')
      call psetr('text_box_y_position', 14.000000)
      call psetc('text_colour', 'black')
      call ptext


      call pclose
      end
