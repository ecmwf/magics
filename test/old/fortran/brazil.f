      program image

	 
	  
      call popen
      call psetc ('device','ps')
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','image.ps')
     
	  call psetc('map_coastline_colour', 'tan')
	  call psetc('map_grid_colour', 'tan')
	  call psetc('map_label_colour', 'tan')
	   call psetr ('ppm_min_latitude',-60.)
       call psetr ('ppm_max_latitude',10.)
       call psetr ('ppm_min_longitude',-80.)
       call psetr ('ppm_max_longitude',-30.)
       
        call psetr ('subpage_lower_left_latitude',    -60.0)
      call psetr ('subpage_lower_left_longitude',  -84.0)
      call psetr ('subpage_upper_right_latitude',   16.0)
      call psetr ('subpage_upper_right_longitude',   -32.0)
	 
	  call pcoast 
	
   

   
     
       
       call psetc ('ppm_input_file_name','../data/brazil.pgm')
      
       call praw
       call psetc  ('symbol_table_mode',  'on')
      call pset1r('symbol_min_table',[-1., 18., 36., 54., 72., 90.], 6)      
      call pset1r('symbol_max_table',[18., 36., 54., 72.,90.,128.], 6)
      
c      call pset1r('symbol_min_table',[-1., 1.], 2)      
c      call pset1r('symbol_max_table',[1.,90.], 2)
      
      call pset1r('symbol_height_table', [0.02], 1)    
      call pset1i('symbol_marker_table', [15], 1)     
      call pset1c('symbol_colour_table', ['navy       ','cream','green',
     +           'evergreen',  'brown', 'white'],6)  
       
       call psymb
c       call praw
c       call pimage
	  

      call pclose
      end
