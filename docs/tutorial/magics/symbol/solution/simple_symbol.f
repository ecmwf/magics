      program  magics

C Define the title
      character*25 var
      dimension var(1)
      data var / +    'My first symbol plotting' /

C Open Magics
      call popen

C set the outut information
      call psetc('output_name', 'simple_symbol')

C Define the background coastlines
      call psetc('map_coastline_sea_shade', 'on')
      call psetc('map_coastline_sea_shade_colour', 'white')
      call psetc('map_label', 'off')
      call psetc('map_coastline_land_shade_colour', 'grey')
      call psetc('map_grid', 'off')
      call psetc('map_coastline_colour', 'black')
      call psetc('map_coastline_land_shade', 'on')
      call pcoast

C Load the data 
      call psetc('geo_input_file_name', '../airep.geo')
      call pgeo

c Define the attributes of the symbol plotting
      call psetc('symbol_type', 'marker')
      call pseti('symbol_marker', 15)
      call psetc('symbol_colour', 'evergreen')
      call psetr('symbol_height', 0.100000)
      call psymb

C Add a text
      call pset1c('text_lines', var, 1)
      call psetc('text_mode', 'positional')
      call psetr('text_box_y_length', 2.500000)
      call psetc('text_justification', 'left')
      call psetc('text_border', 'on')
      call psetr('text_box_x_length', 20.000000)
      call psetr('text_box_x_position', 1.500000)
      call psetc('text_html', 'true')
      call psetc('text_box_blanking', 'on')
      call psetc('text_border_colour', 'black')
      call psetr('text_box_y_position', 13.500000)
      call psetc('text_colour', 'black')
      call ptext


C Close magics : do the plot
      call pclose

      end
