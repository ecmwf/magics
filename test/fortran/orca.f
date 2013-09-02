      program orca

    
      character*20 ctable(6)
      integer mtable(1)
      real   htable(1)
      real min(6), max(6)
      
      
      
      
      data min /-5., 5., 10., 15., 20., 25./         
      data max /5., 10., 15, 20, 15., 50./      
      data htable /0.05/    
      data ctable /'navy', 'blue', 'green',
     +            'yellow', 'orange', 'red'/
     
      data mtable /3/
      
C      data dims 
C     +/'channel/100', 
C     +'parameter/2',   
C     +'longitude/30',
C     + 'levelist/500'/
C     
      


       call popen
      
   
       call psetc ('ps_device','ps_a4')
       call psetc ('ps_file_name','orca.ps')
       call psetc ('legend','on')
       
c     pass the data to magics
       call psetc('netcdf_filename', '../data/meshmask_ORCA_R2_opa9.nc')
       call psetc('netcdf_type', 'orca');
       call psetc('netcdf_field_variable_name', 'mbathy');
       call psetc('netcdf_longitude_variable_name', 'nav_lon');
       call psetc('netcdf_latitude_variable_name', 'nav_lat');
       
       call pnetcdf      



c     set up the coastline attributes
      call psetc ('map_coastline',        'on')
      call psetc ('map_coastline_colour', 'grey')
      call psetc ('map_grid_colour',      'grey')     


   
      
c     define the contour        
      call psetc('symbol_table_mode', 'on')
      call pset1r('symbol_min_table', min, 6)
      call pset1r('symbol_max_table', max, 6)
      call pset1r('symbol_height_table', htable, 1)    
      call pset1i('symbol_marker_table', mtable, 1)     
      call pset1c('symbol_colour_table', ctable, 6)  

      call psymb
      

      call pcoast

      call pclose

      stop
      end
