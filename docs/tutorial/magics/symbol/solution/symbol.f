      program  magics

C Define teh variable to handle the title lines.
      character*25 title
      dimension title(1)
      data title / 'Monitoring of airep data' /

C OPen Magics
      call popen

C Set the output information
      call psetc('output_name', 'symbol')

C Set the coastlines 
      call psetc('map_coastline_sea_shade', 'on')
      call psetc('map_coastline_sea_shade_colour', 'white')
      call psetc('map_label', 'off')
      call psetc('map_coastline_land_shade_colour', 'grey')
      call psetc('map_grid', 'on')
      call psetc('map_coastline_colour', 'black')
      call psetc('map_coastline_land_shade', 'on')
      call pcoast

C Set a legend
      call psetc('legend_text_colour', 'black')
      call psetc('legend_display_type', 'continuous')
      call psetc('legend', 'on')

C Load the data
      call psetc('geo_input_file_name', '../airep.geo')
      call pgeo

c Define the visualisation using the advanced symbol plotting
      call psetc('symbol_advanced_table_selection_type', 'interval')
      call psetc('symbol_advanced_table_min_level_colour', 'blue')
      call psetc('symbol_table_mode', 'advanced')
      call psetc('symbol_type', 'marker')
      call psetr('symbol_advanced_table_interval', 5.000000)
      call psetc('symbol_advanced_table_colour_direction', 'clockwise')
      call pseti('symbol_marker', 15)
      call psetc('symbol_advanced_table_max_level_colour', 'red')
      call psymb

C Add  a title
      call psetc('text_mode', 'positional')
      call psetr('text_font_size', 0.600000)
      call psetc('text_justification', 'left')
      call psetc('text_border', 'off')
      call pset1c('text_lines', title, 1)
      call psetr('text_box_x_length', 20.000000)
      call psetr('text_box_y_length', 2.500000)
      call psetc('text_html', 'true')
      call psetr('text_box_x_position', 1.500000)
      call psetr('text_box_y_position', 16.500000)
      call psetc('text_colour', 'black')
      call ptext


C Close Magics : execute the plot
      call pclose

      end
