 
      program obs1
      
      character*5 obtyp(1)
      data obtyp /'synop'/
c
c     open magics and define postscript device  
c     output file name: output.ps
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'obs_1.ps')
c
c     set up file containing obervations
c
      call psetc('obs_input_file_name',
     +'data/obs_tc.bfr')
      call psetc('map_coastline_colour','grey')
      call psetc('map_grid','off') 
      call psetc('map_label','off')
      call pcoast        
         
c
c     synop observations plotted with identifiers
c
      call pset1c('obs_type_list',obtyp,1)                            
      call psetr('obs_distance_apart',2.0)                           
      call psetc('obs_pressure_colour','blue')                       
      call psetc('obs_identification','on')                         
      call pobs
      



      call pclose
      
      end
      
