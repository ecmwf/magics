      program logo
	

	  
      call popen
      call psetc ('device','ps')
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','logo.ps')
     
      
      call psetc('user_logo_filename', '../data/logo_mf_vertical.gif')
      call psetr('user_logo_x_position', 23.7)
      call psetr('user_logo_width', 1.)
      call psetr('user_logo_y_position', .2)
      call psetr('user_logo_height', 2.)
	  call pimport
      call pcoast
	  

      call pclose
      end
