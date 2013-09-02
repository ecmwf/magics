 
      program obs2
      
      character*5 obtyp(1)
      data obtyp /'temp'/
c
c     open magics and define postscript device  
c     output file name: output.ps
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'obs_2.ps')  
      call pseti ('map_coastline_thickness', 2)
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
      call psetc ('map_grid_line_style','dash')
      
      call pcoast     
      
c
c     set up file containing obervations
c
      call psetc('obs_input_file_name',
     +'data/obs_tc.bfr')
      call pset1c('obs_type_list',obtyp,1)    
 
c
c     plot temps at level 300 hpa with 850/300 thicknesses
c
      call psetc('obs_thickness','on')                         
      call pseti('obs_level',300)                           
      call pseti('obs_level_2',850)                           
      call pset1c('obs_type_list',obtyp,1)                              
      call pobs
c
c     Add some text
c     
      call psetc ('text_line_1', 'Plotting Temp Observations...')
      call psetc ('text_colour', 'navy')
      call pseti ('text_line_count', 1)
      call psetr ('text_reference_character_height', 0.5)
      call psetc ('text_justification', 'left')
      call ptext


      call pclose
      
      end
      
