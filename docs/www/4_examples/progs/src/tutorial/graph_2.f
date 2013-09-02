      program graph_2
		
	  character *20 dates
      dimension dates(9)
      real values(9), orig(9)
      data dates /'2007-02-25 00:00:00', 
     +            '2007-02-25 06:00:00',
     +            '2007-02-25 12:00:00',
     +            '2007-02-25 18:00:00',
     +            '2007-02-26 00:00:00',
     +            '2007-02-26 06:00:00',
     +            '2007-02-26 12:00:00',
     +            '2007-02-26 18:00:00',
     +            '2007-02-27 00:00:00'/
      data values / 5., 10., 4., 7., 12., 2., 7., 9., 1./
      
      call popen
      call psetc ('ps_device',    'ps_a4')
      call psetc ('ps_file_name', 'graph_2.ps')

      call psetc('page_frame','off')  
    
      call psetc('subpage_map_projection','none')  
      call pseti('subpage_frame_thickness',2)  
      call psetc('page_id_line','on')
      
      
      

c-------- plot vertical axis ----------------------------
            
             
     
      call psetc('axis_orientation','horizontal')
      call psetc('axis_position','bottom')
      call psetc('axis_type','date')
      call psetc('axis_grid','off')
      call psetc('axis_line_colour','black')
      call psetc('axis_date_type','days')
      call psetc('axis_date_min_value','2007-02-25 00:00:00')
      call psetc('axis_date_max_value', '2007-02-27 00:00:00')
      call psetc('axis_tick','on')
      call psetr('axis_tick_interval',1.0)
      call psetc('axis_days_label_colour','black')
      call psetc('axis_days_label','number')
      call psetc('axis_line','on')
      
     
      call paxis

       
       call psetc('axis_orientation','vertical')
       call psetc('axis_type','regular')
      
       call psetr('axis_min_value',0.0)
       call psetr('axis_max_value',15.0)

       call psetr('axis_tick_interval',5.0)

       call pseti('axis_tick_label_frequency',2)
       call psetc('axis_tick_label_quality','high')


       call psetc('axis_tick_label_colour','black')

       call psetc('axis_line','on')

C
c--- Add a title
c
       call psetc('axis_title','on')
       call psetc('axis_title_text','My Unit')
       call psetc('axis_title','on')
       call psetr('axis_title_height',0.6)
       call psetc('axis_title_colour','black')
       call psetc('axis_title_orientation','parallel')
c            
c---- gridding lines

        call psetc('axis_grid','on')
        call psetc('axis_grid_colour','black')
        call pseti('axis_grid_thickness',2)
        call psetc('axis_grid_line_style','dot')

 
        call paxis
   

  
c-------- setting the bar style ----------------------------


      call psetc ('graph_type','bar')
	  call psetc ('graph_shade','on')
	  call psetc ('graph_shade_style','area_fill')

	  call psetc ('graph_shade_colour','grey')
      call psetc ('graph_bar_colour','grey')
      call psetr ('graph_bar_width', 0.8)
      call psetc ('graph_line_colour','black')
      call pseti ('graph_line_thickness', 4)
	  call pset1c ('graph_bar_date_x_values',dates, 9)
	  call pset1r ('graph_bar_y_upper_values',values, 9)
	 
      call psetc('legend','on')
      call psetc('legend_user_text','My data')
      call psetc('legend_text_colour','navy')
      call psetr('legend_text_height',0.5)
      call pgraph
     

       call pclose

       end





c --------------------------------------------------------------------
c     parse_command_line
c     checks the command-line for any arguments.
c     arguments can come in pairs. currently supported arguments are:
c     projection 
c     device 
c     e.g. run the program with:
c      projection polar_stereographic
c      projection cylindrical   device svg
c --------------------------------------------------------------------
