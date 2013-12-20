      program multidriver
c
     
      
      call popen
      call pset1c ('devices',['svg', 'ps', 'gd'], 3)
      
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','coast.ps')
      call psetc ('gd_file_name','coast.gif')
      call psetc ('svg_file_name','coast.svg')
      
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
    
      call psetc ('map_grid','on')   
      call pcoast
	  call pclose
      stop
      end
