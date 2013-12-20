      program image

	 
	  
      call popen
      call psetc ('device','ps')
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','image.ps')
     
	  call psetc('map_coastline_colour', 'tan')
	  call psetc('map_grid_colour', 'tan')
	  call psetc('map_label_colour', 'tan')
	   call psetr ('ppm_min_latitude',-90.)
       call psetr ('ppm_max_latitude',90.)
       call psetr ('ppm_min_longitude',-180.)
       call psetr ('ppm_max_longitude',180.)
       
     
	 
	  call pcoast 
	
   

   
     
       
       call psetc ('ppm_input_file_name','../data/globe.pgm')
      
       call praw
        
       call psetc('image_colour_table_creation_mode', 'equidistant')
       call pimage

	  

      call pclose
      end
