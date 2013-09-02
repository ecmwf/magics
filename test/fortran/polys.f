      program polys
    
      call popen
c      call psetc('legend_box_mode', 'positional')
c      call psetr('legend_box_x_position', 25.)
c      call psetr('legend_box_y_position', 7.)
c      call psetr('legend_box_x_length', 5.)
c      call psetr('legend_box_y_length', 10.)
c     call psetc('legend_box_blanking', 'on')
c      call psetc('legend_box_border', 'on')
      call psetc('legend_title', 'on')
      call psetc('legend_title_text', 'My title')
      call psetc('legend_display_type', 'continuous')
      call psetc('legend_text_format', '(f5.2)')

      
        call psetr ('subpage_lower_left_longitude',  -30.0)
        call psetr ('subpage_upper_right_longitude',  77.0)
        call psetr ('subpage_upper_right_latitude',   75.0)
        call psetr ('subpage_lower_left_latitude',    25.0)
        call psetc('legend', 'on')
        
        call psetc('polyline_input_positions_filename', 
     +         '../data/catchment.pos')
        call psetc('polyline_input_values_filename', 
     +         '../data/catchment.val')
        call psetr('polyline_input_break_indicator', -999.)
        call psetc('polyline_shade', 'on')
        call psetc('polyline_shade_colour_method',  'calculate')
        call psetc('polyline_shade_max_level_colour', 'red')
       
        call psetc('polyline_shade_min_level_colour', 'blue')
        call psetc('polyline_shade_colour_direction', 'clockwise')
        call pline
      call pcoast
       call pnew("page")
      call psetc('legend_box_mode', 'positional')
      call psetr('legend_box_x_position', 25.)
      call psetr('legend_box_y_position', 7.)
      call psetr('legend_box_x_length', 5.)
       call psetr('polyline_shade_max_level', 8.)
        call psetr('polyline_shade_min_level', 4.)
      call psetr('legend_box_y_length', 10.)
      call psetc('legend_box_blanking', 'on')
      call psetc('legend_box_border', 'on')
       call psetc('polyline_shade_level_selection_type', 'interval')
       call psetr('polyline_interval', 0.5)
     
      call pline
      call pcoast
      
    
      call pclose
    
      end
