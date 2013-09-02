      program  magics

C Defines the lines for the title
      character*25 title
      dimension title(2)

      data title /
     +    'My first line', 
     +    'My second line' /

C open Magics
      call popen

C setup the output name
      call psetc('output_name', 'msl_uv_australia')

C Setup the geographical area : here we use the Australia definition
      call psetr('subpage_lower_left_longitude', 80.000000)
      call psetr('subpage_upper_right_longitude', 190.000000)
      call psetr('subpage_upper_right_latitude', -5.000000)
      call psetc('subpage_map_projection', 'cylindrical')
      call psetr('subpage_lower_left_latitude', -55.000000)

C Setup the backgrounf coastlines
      call psetc('map_coastline_sea_shade', 'on')
      call psetc('map_coastline_sea_shade_colour', 'white')
      call psetc('map_label', 'off')
      call psetc('map_coastline_land_shade_colour', 'cream')
      call psetc('map_grid', 'off')
      call psetc('map_coastline_colour', 'tan')
      call psetc('map_coastline_land_shade', 'on')
      call pcoast

C Import the wind speed data
      call psetc('grib_input_file_name', '../speed200.grb')
      call pgrib

C Define the shading for the wind speed
      call psetc('legend', 'on')
      call psetc('contour_level_selection_type', 'level_list')
      call psetc('contour_shade_method', 'area_fill')
      call psetc('contour_shade', 'on')
      call pset1r('contour_level_list', 
     +           (/30., 40., 50., 60., 70., 80., 90., 100./), 8)
      call psetr('contour_hi_min_value', 15.000000)
      call psetr('contour_reference_level', 0.000000)
      call psetc('contour_hilo', 'on')
      call psetc('contour_hilo_format', '(F3.0)')
      call psetr('contour_hilo_height', 0.200000)
      call psetc('contour_highlight', 'off')
      call psetc('contour_label', 'off')
      call psetc('contour_hilo_type', 'number')
      call psetr('contour_hilo_suppress_radius', 30.000000)
      call psetc('contour_shade_max_level_colour', 'evergreen')
      call psetc('contour_shade_min_level_colour', 'yellow')
      call pcont


C Import the wind data
      call psetc('grib_input_file_name', '../uv200.grb')
      call pgrib

C Define the visualisation of the wind
      call pseti('wind_arrow_thickness', 1)
      call psetr('wind_thinning_factor', 3.000000)
      call psetc('wind_arrow_colour', 'gold')
      call psetc('wind_field_type', 'arrows')
      call pwind

C Import the Msl data
      call psetc('grib_input_file_name', '../msl.grb')
      call pgrib


C Define the visualisation of the msl
      call psetc('contour_level_selection_type', 'interval')
      call psetc('contour_shade', 'off')
      call psetc('legend', 'off')
      call psetc('contour_line_colour', 'black')
      call psetc('contour_lo_colour', 'black')
      call pseti('contour_highlight_thickness', 2)
      call psetc('contour_hi_colour', 'black')
      call psetc('contour_hilo', 'on')
      call psetr('contour_hilo_height', 0.250000)
      call pseti('contour_line_thickness', 1)
      call psetc('contour_label', 'off')
      call psetc('contour_highlight_colour', 'black')
      call psetc('contour_hilo_quality', 'high')
      call psetr('contour_interval', 5.000000)
      call pcont


      call psetc('map_label', 'off')
      call psetc('map_grid_colour', 'tan')
      call psetc('map_grid', 'on')
      call psetc('map_coastline_colour', 'tan')
      call psetc('map_coastline_land_shade', 'off')
      call psetc('map_coastline_sea_shade', 'off')
      call pcoast

C add the text
      call pset1c('text_lines', title, 2)
      call psetc('text_mode', 'positional')
      call psetr('text_box_y_length', 2.500000)
      call psetc('text_justification', 'left')
      call psetc('text_border', 'on')
      call psetr('text_box_x_length', 20.000000)
      call psetr('text_box_x_position', 1.500000)
      call psetc('text_html', 'true')
      call psetc('text_box_blanking', 'on')
      call psetc('text_border_colour', 'black')
      call psetr('text_box_y_position', 12.000000)
      call psetc('text_colour', 'black')
      call ptext

C Add a legend
      call psetr('legend_box_x_position', 27.000000)
      call psetc('legend_display_type', 'continuous')
      call psetc('legend_box_blanking', 'on')
      call psetr('legend_box_y_position', 0.750000)
      call psetr('legend_box_x_length', 2.000000)
      call psetc('legend_border', 'on')
      call psetc('legend_border_colour', 'black')
      call psetc('legend_box_mode', 'positional')
      call psetc('legend_text_colour', 'black')
      call psetr('legend_box_y_length', 12.000000)
      call psetc('legend', 'on')


      call pclose

      end
